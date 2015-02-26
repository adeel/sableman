(ns sableman.export.tex
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [hiccup.core :as hiccup]
            [digest])
  (:use [slingshot.slingshot :only [throw+]]
        [fleet :only [fleet]]
        [me.raynes.fs :only [delete temp-dir]])
  (:require [sableman.bundle.path :as bp]
            [sableman.bundle.meta :as bmet]
            [sableman.bundle.document :as bd]
            [sableman.tagdef :as tagdef]))

(def paragraph-node->tex
  (fleet [node-type content]
    (slurp (io/resource "templates/tex/paragraph.tex"))))

(def block-node->tex
  (fleet [node-type content]
    (slurp (io/resource "templates/tex/block.tex"))))

(def list-node->tex
  (fleet [list-type content]
    (slurp (io/resource "templates/tex/list.tex"))))

(def item-node->tex
  (fleet [node-type content]
    (slurp (io/resource "templates/tex/item.tex"))))

(defn node-map->tex [node doc-idx bun-meta]
  (if (string? node)
    node
    (let [node-class (name (node :type))
          content    (reduce str
                             (map #(node-map->tex % doc-idx bun-meta)
                                  (node :content)))]
      (cond
       (contains? tagdef/paragraph-tags (node :type))
         (paragraph-node->tex node-class content)
       (contains? tagdef/block-tags (node :type))
         (if (= :text (node :type))
           (str "\n" content "\n")
           (block-node->tex node-class content))
       (= :def (node :type))
         (str "\\emph{" content "}")
       (contains? #{:term :notation} (node :type))
         (str "\\emph{" content "}")
       (= :citation (node :type))
         (cond
          (map? (node :ref))
            (if (= (get-in node [:ref :bun-name]) (bun-meta :name))
              (str "(" (get-in node [:ref :doc-index])
                   "." (get-in node [:ref :par-index]) ")")
              (str "(" (get-in node [:ref :bun-name])
                   "/" (get-in node [:ref :doc-index])
                   "." (get-in node [:ref :par-index]) ")"))
          (map? (node :formula-ref))
            (if (= (get-in node [:formula-ref :bun-name]) (bun-meta :name))
              (str "(" (get-in node [:formula-ref :doc-index])
                   "." (get-in node [:formula-ref :fml-index]) ")")
              (str "(" (get-in node [:formula-ref :bun-name])
                   "/" (get-in node [:formula-ref :doc-index])
                   "." (get-in node [:formula-ref :fml-index]) ")"))
          (not (string/blank? (node :ext-ref)))
            (string/join
             ""
             (filter identity
                     ["("
                      (when (node :pre-note)
                        (str (node :pre-note) " "))
                      (node :ext-ref)
                      (when (node :post-note)
                        (str ", " (node :post-note)))
                      ")"])))
       (= :formula (node :type))
         (str "\\[" content (if (node :formula-index)
                              (str "\\tag{"
                                   (if (str doc-idx ".") doc-idx "")
                                   (get node :formula-index 0) "}")
                              "")
              "\\]")
       (= :inline-formula (node :type))
         (str "\\(" content "\\)")
       (= :list (node :type))
         (list-node->tex (node :list-type) content)
       (= :item (node :type))
         (item-node->tex node-class content)
       (= :italics (node :type))
         (str "{\\itshape " content "}")
       (= :bold (node :type))
         (str "{\\bfseries " content "}")
       (contains? tagdef/inline-tags (node :type))
         content))))

;;;;

(defn document-map->tex [doc-map doc-idx bun-meta]
  (reduce str (map #(node-map->tex % doc-idx bun-meta) (doc-map :body))))

(defn export-document-to-tex [doc-name doc-idx bun-meta]
  (let [doc-map    (bd/read-document doc-name bun-meta)
        doc-map    (bd/resolve-refs doc-map doc-name bun-meta)
        doc-tex    (document-map->tex doc-map doc-idx bun-meta)]
    (assoc doc-map :tex doc-tex)))

(defn export-intro-to-tex [bun-meta]
  (export-document-to-tex "intro" nil bun-meta))

(def bundle->tex
  (fleet [bun-meta intro docs]
    (slurp (io/resource "templates/tex/bundle.tex"))))

(defn export-bundle-to-tex! [bun-path]
  (let [bun-meta     (bmet/read-meta bun-path)
        bun-meta     (bmet/load-deps bun-meta)
        bun-meta     (bmet/load-titles bun-meta)
        bun-meta     (bmet/load-ext-refs bun-path bun-meta)
        missing-deps (filter (fn [[k v]] (not (v :path)))
                             (bun-meta :deps-map))
        intro        (if (bp/intro-exists? bun-path)
                       (export-intro-to-tex bun-meta)
                       {:title "" :description "" :tex ""})
        docs         (map-indexed
                      (fn [i doc-name]
                        (export-document-to-tex doc-name (inc i) bun-meta))
                      (bun-meta :contents))
        tex-str      (bundle->tex bun-meta intro docs)]
    (if (seq missing-deps)
      (throw+ {:type ::missing-deps
               :missing-deps missing-deps}))
    (with-open [w (io/writer (bp/get-tex-file bun-path bun-meta))]
      (.write w (str tex-str)))
    (with-open [w (io/writer (str (bp/get-tex-dir bun-path) "/sable.sty"))]
      (.write w (slurp (io/resource "templates/tex/sable.sty"))))))

;(def bun-path
;  (str (-> (java.io.File. ".") .getAbsolutePath) "/resources/sample"))
;(def bun-meta (bmet/read-meta bun-path))
;(def doc-map (bd/read-document "firstdoc" bun-meta))
;(document-map->tex doc-map bun-meta)

;(export-tex "firstdoc" bun-path)

;(export-document-to-tex "mtvspc" bun-path)
;(export-bundle-to-tex! "/home/adeel/Dropbox/Math/notes/mtvhtp")
;(export-bundle-to-tex! "/home/adeel/Dropbox/Math/notes/presheaves")
;(export-bundle-to-tex! "/home/adeel/Dropbox/Math/notes/mtvhtp-sixops")
