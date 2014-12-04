(ns sableman.export.html
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [hiccup.core :as hiccup]
            [net.cgrand.enlive-html :as enlive]
            [digest])
  (:use [slingshot.slingshot :only [throw+]]
        [fleet :only [fleet]]
        [me.raynes.fs :only [delete temp-dir]])
  (:require [sableman.bundle.path :as bp]
            [sableman.bundle.meta :as bmet]
            [sableman.bundle.document :as bd]
            [sableman.tagdef :as tagdef]))

(defn- ref-map->str [ref bun-meta]
  (string/join
   (concat
    (when (ref :bun-name)
      [(ref :bun-name) "/"])
    (when (ref :doc-index)
      [(ref :doc-index)])
    (when (and (ref :par-index) (not= 0 (ref :par-index)))
      ["." (ref :par-index)]))))

(defn formula->png [formula bun-meta]
  (let [formula   (str "\\[" formula "\\]")
        frml-tmpl (fleet [formula preamble font-size] (slurp (io/resource "templates/formula.tex")))
        tex       (str (frml-tmpl formula (get bun-meta :tex-preamble "") "12"))
        tex-hash  (digest/sha-256 tex)
        tex-path  (str bp/formula-cache-dir "/" tex-hash ".tex")
        pdf-path  (str bp/formula-cache-dir "/" tex-hash ".pdf")
        png-path  (str bp/formula-cache-dir "/" tex-hash ".png")]
    (if-not (.exists (io/file png-path))
      (do (spit tex-path tex)
          (shell/sh "pdflatex"
                    "-interaction=batchmode"
                    (str "-output-dir=" bp/formula-cache-dir)
                    tex-path)
          (shell/sh "convert"
                    "-density" "110"
                    "-trim"
                    "-transparent" "#FFFFFF"
                    pdf-path png-path)))
    (if-let [idout (shell/sh "identify" "-format" "%wx%h" png-path)]
      (when (= 0 (:exit idout))
        (delete tex-path)
        (delete pdf-path)
        [(str tex-hash ".png")
         (string/split (:out idout) #"x")]))))

(defn- expand-inline-formulas [text bun-meta]
  (if-let [formulas (re-seq #"([^$]*)\$([^$]*)\$([^$]*)" text)]
    (apply concat
     (map (fn [m]
            [(get m 1)
             {:type    :inline-formula
              :content (get m 2)}
             (get m 3)])
          formulas))
    [text]))

(defn node-map->html [node doc-idx bun-meta]
  (if (string? node)
    (map (fn [n]
           (if (string? n)
             n
             (node-map->html n doc-idx bun-meta)))
      (expand-inline-formulas node bun-meta))
    (let [node-class (name (node :type))]
      (cond
       (contains? tagdef/paragraph-tags (node :type))
         [:div.paragraph {:class node-class :id (node :index)}
          [:h3.paragraph-type
           [:a {:href (str "#" (node :index))}
            (if (= :paragraph (node :type))
              "&para;"
              (string/capitalize (name (node :type))))
            " " doc-idx "." (get node :index "")]]
          (map #(node-map->html % doc-idx bun-meta) (node :content))]
       (contains? tagdef/block-tags (node :type))
         [(if (= :statement (node :type)) :div :p) {:class node-class}
          (map #(node-map->html % doc-idx bun-meta) (node :content))]
       (= :def (node :type))
         [:span.def
          (map #(node-map->html % doc-idx bun-meta) (node :content))]
       (contains? #{:term :notation} (node :type))
         [:span {:class node-class}
          (cond
           (map? (node :ref))
             [:a {:href (str "../../../"
                              (get-in node [:ref :bun-author]) "/"
                              (get-in node [:ref :bun-name]) "/"
                              (get-in node [:ref :bun-version]) "/"
                              (get-in node [:ref :doc-name]) ".html#"
                              (get-in node [:ref :par-index]))}
              (map #(node-map->html % doc-idx bun-meta) (node :content))]
           (not (string/blank? (node :ext-ref)))
             [:a {:href (str "#" (node :ext-ref))}
              (map #(node-map->html % doc-idx bun-meta) (node :content))]
           :else
             (map #(node-map->html % doc-idx bun-meta) (node :content)))]
       (= :citation (node :type))
         [:span {:class node-class}
          (cond
           (map? (node :ref))
             [:span
              "("
              [:a {:href (str "../../../"
                              (get-in node [:ref :bun-author]) "/"
                              (get-in node [:ref :bun-name]) "/"
                              (get-in node [:ref :bun-version]) "/"
                              (get-in node [:ref :doc-name]) ".html#"
                              (get-in node [:ref :par-index]))}
               (ref-map->str (node :ref) bun-meta)]
              ")"]
           (not (string/blank? (node :ext-ref)))
             [:span {:class :ext-ref}
              "("
              (if (node :pre-note)
                (str (node :pre-note) " "))
              [:a {:href (str "refs.html#" (node :ext-ref))}
               (node :ext-ref)]
              (if (node :post-note)
                (str ", " (node :post-note)))
              ")"]
           :else
             (map #(node-map->html % doc-idx bun-meta) (node :content)))]
       (contains? #{:formula :inline-formula} (node :type))
         (let [formula  (string/join (map str (node :content)))
               png      (formula->png formula bun-meta)]
           (if png
             (let [[fname [width height]] png]
               [(if (= :formula (node :type)) :div.formula :span.inline-formula)
                [:img {:src    (str "../../../.formulas/" fname)
                       :alt    formula
                       :width  width
                       :height height}]])
             formula))
       (= :text (node :type))
         [:p {:class node-class}
          (map #(node-map->html % doc-idx bun-meta) (node :content))]
       (= :list (node :type))
         (let [list-type (node :list-type)
               list-el   (if (contains? #{"disc" "circle" "square"} list-type)
                           :ul
                           :ol)]
           [list-el {:class node-class :type list-type}
            (map #(node-map->html % doc-idx bun-meta) (node :content))])
       (= :item (node :type))
         [:li {:class node-class}
          (map #(node-map->html % doc-idx bun-meta) (node :content))]
       (contains? tagdef/inline-tags (node :type))
         [:span {:class node-class}
          (map #(node-map->html % doc-idx bun-meta) (node :content))]))))

(defn- render-latex-str [s bun-meta]
  "use this for rendering titles and descriptions"
  (map (fn [n]
           (if (string? n)
             n
             (node-map->html n 0 bun-meta)))
       (expand-inline-formulas (str s) bun-meta)))

;;;;;; templates

(enlive/deftemplate document-tmpl "templates/document.html"
  [doc-map doc-idx bun-meta]
  [:title]
    (enlive/content (doc-map :title))
  [:.bundle]
    ;; todo: html escaping issue?
    (enlive/html-content
     (hiccup/html
      (render-latex-str (bun-meta :title) bun-meta)))
  [:.index]
    (enlive/content (str doc-idx))
  [:.title]
    (enlive/html-content
     (hiccup/html
      (render-latex-str (doc-map :title) bun-meta)))
  [:.description]
    (enlive/html-content
     (hiccup/html
      (render-latex-str (doc-map :description) bun-meta)))
  [:.body]
    (enlive/html-content
     (hiccup/html (map #(node-map->html % doc-idx bun-meta) (doc-map :body)))))

(defn document-map->html [doc-map doc-idx bun-meta]
  (reduce str (document-tmpl doc-map doc-idx bun-meta)))

(enlive/deftemplate index-tmpl "templates/index.html"
  [bun-meta]
  [:title]
    (enlive/content (bun-meta :title))
  [:.name]
    (enlive/content (str (bun-meta :author) "/" (bun-meta :name)))
  [:.title]
    (enlive/html-content
     (hiccup/html
      (render-latex-str (bun-meta :title) bun-meta)))
  [:.description]
    (enlive/html-content
     (hiccup/html
      (render-latex-str (bun-meta :description) bun-meta)))
  [:.toc]
    (enlive/html-content
     (hiccup/html
      (for [[doc-idx doc-name doc-title] (bun-meta :titles)]
       [:div.document
        (str "&sect; " doc-idx ". ")
        [:a {:href (str doc-name ".html")}
         (render-latex-str doc-title bun-meta)]])))
  [:.deps]
    (enlive/html-content
     (hiccup/html
      (for [[_ d] (bun-meta :deps-map)]
        [:div.dep
         [:a {:href (str "../../../"
                             (d :author) "/"
                             (d :name) "/"
                             (d :version) "/")}
          (d :author)
          "/"
          (d :name)
          "/"
          (d :version)]]))))

(enlive/deftemplate refs-tmpl "templates/refs.html"
  [bun-meta]
  [:title]
    (enlive/content (bun-meta :title))
  [:.bundle]
    (enlive/html-content
     (hiccup/html
      (render-latex-str (bun-meta :title) bun-meta)))
  [:.references]
    (enlive/html-content
     (hiccup/html
      (map-indexed
       (fn [idx [ref-id ref]]
         [:div.ref {:id ref-id}
          [:span.ref-id ref-id]
          [:span.author (ref :author)]
          ". "
          [:span.title (ref :title)]
          "."])
       (bun-meta :ext-refs)))))

;; (defn document-map->html [doc-map bun-meta]
;;   (html5
;;    [:head$
;;     [:title (doc-map :title)]
;;     [:script {:type "text/x-mathjax-config"}
;;      "MathJax.Hub.Config({"
;;        "tex2jax: {inlineMath: [ ['$','$'] ]},"
;;        "TeX: {Macros: {"
;;          (for [m bun-macros]
;;            (str (m :name) ": \"" (string/escape (m :value)
;;                                                 {\\ "\\\\"}) "\","))
;;        "}}"
;;      "});"]
;;     [:script {:type "text/javascript"
;;               :src  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"}]]
;;    [:body
;;     [:div.description
;;      [:p (doc-map :description)]]
;;     (for [par (doc-map :body)]
;;       (node-map->html par bun-meta))]))

;;;;;;;;;;;;;;

(defn export-document-to-html! [doc-name doc-idx bun-meta]
  (let [doc-map    (bd/read-document doc-name bun-meta)
        doc-map    (bd/resolve-refs doc-map doc-name bun-meta)
        doc-html   (document-map->html doc-map doc-idx bun-meta)]
    (with-open [w (io/writer (bp/get-html-file doc-name bun-meta))]
      (.write w doc-html))))

(defn export-bundle-to-html! [bun-path]
  (let [bun-meta     (bmet/read-meta bun-path)
        bun-meta     (bmet/load-deps bun-meta)
        bun-meta     (bmet/load-titles bun-meta)
        bun-meta     (bmet/load-ext-refs bun-path bun-meta)
        missing-deps (filter (fn [[k v]] (not (v :path)))
                             (bun-meta :deps-map))
        index-html   (reduce str (index-tmpl bun-meta))
        refs-html    (reduce str (refs-tmpl bun-meta))
        css          (slurp (io/resource "templates/style.css"))]
    (if (seq missing-deps)
      (throw+ {:type ::missing-deps
               :missing-deps missing-deps}))
    (with-open [w (io/writer (bp/get-html-index bun-meta))]
      (.write w index-html))
    (with-open [w (io/writer (bp/get-html-refs bun-meta))]
      (.write w refs-html))
    (with-open [w (io/writer (bp/get-html-css bun-meta))]
      (.write w css))
    (doall
     (map-indexed (fn [i doc-name]
                    (println "Exporting" doc-name "...")
                    (export-document-to-html! doc-name (inc i) bun-meta))
                  (bun-meta :contents)))))

;(def bun-path
;  (str (-> (java.io.File. ".") .getAbsolutePath) "/resources/sample"))
;(def bun-meta (bmet/read-meta bun-path))
;(def doc-map (bd/read-document "firstdoc" bun-meta))
;(document-map->html doc-map bun-meta)

;(export-html "firstdoc" bun-path)

;(def bun-path "/home/adeel/Dropbox/Math/notes/mtvhtp-sixops")
;(export-document-to-html! "dirimgclimm" bun-path)
;(export-document-to-html "mtvspc" bun-path)
;(export-bundle-to-html! "/home/adeel/Dropbox/Math/notes/mtvhtp")
;(export-bundle-to-html! "/home/adeel/Dropbox/Math/notes/presheaves")
;(export-bundle-to-html! "/home/adeel/Dropbox/Math/notes/mtvhtp-sixops")
