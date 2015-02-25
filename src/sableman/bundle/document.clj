(ns sableman.bundle.document
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip :as data-zip]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.data.xml :as xml])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [sableman.bundle.path :as bp]
            [sableman.bundle.meta :as bm]
            [sableman.tagdef :as tagdef])
  (:use [sableman.util]))

;; (defn list [bun-path bun-meta]
;;   (bun-meta :contents))

;(defn find-par-by-name [name-str doc-map]
;  (filter
;    (fn [i par-names] (contains? (set par-names) name-str))
;    (map-indexed
;      (fn [i par-map]
;        (let [par-names-str (get par-map :name "")
;              par-names-vec (map string/trim (string/split par-names-str #";"))]
;          [(inc i) par-names-vec]))
;      (doc-map :body))))

; document parsing
;;;;;;;;;;;;;;;;;;

(declare read-document)

;;;;;;;;; parsing functions

; a counter for named formulas

(def *formula-counter* (atom 0))

(defn inc-formula-counter! []
  (swap! *formula-counter* inc))

(defn reset-formula-counter! []
  (reset! *formula-counter* 0))

(def letters
  (map char (concat (range 97 123) (range 65 91))))

(defn integer->letter [n]
  (nth letters (dec n)))

;; parsing strategy:
; need a function that, given :paragraph or :formula or :proof,
; returns the subset of tags that are allowed within that tag
; one can have a generic parse-node function which parses only
; the children tags which are in this subset

(declare get-node-content)

;; node->map functions

(defn ref-str->map [ref-str doc-name bun-name]
  (let [ref-parts (string/split ref-str #"/" -1)] ; -1 as the limit argument makes it not ignore empty strings at the end
    (case (count ref-parts)
          0  {}
          1  {:bun-name bun-name
              :doc-name doc-name
              :par-name (get ref-parts 0)}
          2  {:bun-name bun-name
              :doc-name (get ref-parts 0)
              :par-name (get ref-parts 1)}
             {:bun-name (get ref-parts 0)
              :doc-name (get ref-parts 1)
              :par-name (get ref-parts 2)})))

(defn formula-ref-str->map [ref-str doc-name bun-name]
  (let [ref-parts (string/split ref-str #"/" -1)] ; -1 as the limit argument makes it not ignore empty strings at the end
    (case (count ref-parts)
          0  {}
          1  {:bun-name bun-name
              :doc-name doc-name
              :fml-name (get ref-parts 0)}
          2  {:bun-name bun-name
              :doc-name (get ref-parts 0)
              :fml-name (get ref-parts 1)}
             {:bun-name (get ref-parts 0)
              :doc-name (get ref-parts 1)
              :fml-name (get ref-parts 2)})))

(defn linked-node->map
  "Expects one of the following:
    * ext-ref='someexternalreference'
    * ref='some paragraph in this document'
    * ref='anotherdocinthisbundle/a paragraph in that document'
    * ref='anotherbundle/somedoc/some paragraph in that doc
  plus optionally :pre-note and/or :post-note."
  [node-el context]
  (let [node-zipper  (zip/xml-zip node-el)
        ref-str      (zip-xml/attr node-zipper :ref)
        ref-map      (when ref-str (ref-str->map ref-str (context :doc-name) (get-in context [:bun-meta :name])))
        fml-ref-str  (zip-xml/attr node-zipper :formula-ref)
        fml-ref-map  (when fml-ref-str (formula-ref-str->map fml-ref-str (context :doc-name) (get-in context [:bun-meta :name])))
        ext-ref      (zip-xml/attr node-zipper :ext-ref)
        pre-note     (zip-xml/attr node-zipper :pre-note)
        post-note    (zip-xml/attr node-zipper :post-note)]
    {:type        (:tag node-el)
     :ref         ref-map
     :formula-ref fml-ref-map
     :ext-ref     ext-ref
     :pre-note    pre-note
     :post-note   post-note
     :content     (get-node-content node-el context)}))

(defn def-node->map [node-el context]
  {:type    :def
   :content (get-node-content node-el context)})

(defn formula-node->map [node-el context]
  (let [f-map   {:type    :formula
                 :content (get-node-content node-el context)}]
    (if (zip-xml/attr (zip/xml-zip node-el) :name)
      (assoc f-map :formula-index (integer->letter (inc-formula-counter!)))
      f-map)))

(defn list-node->map [node-el context]
  (let [node-zipper (zip/xml-zip node-el)
        list-type   (zip-xml/attr node-zipper :type)]
    {:type      :list
     :content   (get-node-content node-el context)
     :list-type (or list-type "disc")}))

(defn list-item-node->map [node-el context]
  {:type    :list-item
   :content (get-node-content node-el context)})

(defn italics-node->map [node-el context]
  {:type    :italics
   :content (get-node-content node-el context)})

(defn bold-node->map [node-el context]
  {:type    :bold
   :content (get-node-content node-el context)})

(defn node->map [node-el context]
  (if (string? node-el)
    node-el
    (let [node-tag     (:tag node-el)
          node-zipper  (zip/xml-zip node-el)
          node-map     {:type node-tag}]
      (cond
        (contains? tagdef/paragraph-tags node-tag)
          (merge node-map
            {:name    (zip-xml/attr node-zipper :name)
             :content (get-node-content node-el context)})
        (= :cite node-tag)
          (assoc (merge node-map (linked-node->map node-el context))
            :type :citation)
        (= :notation node-tag)
          (merge node-map (linked-node->map node-el context))
        (= :term node-tag)
          (merge node-map (linked-node->map node-el context))
        (= :def node-tag)
           (merge node-map (def-node->map node-el context))
        (= :f node-tag)
           (merge node-map (formula-node->map node-el context))
        (= :list node-tag)
           (merge node-map (list-node->map node-el context))
        (= :list-item node-tag)
           (merge node-map (list-item-node->map node-el context))
        (= :i node-tag)
           (merge node-map (italics-node->map node-el context))
        (= :b node-tag)
           (merge node-map (bold-node->map node-el context))
        :else
          (assoc node-map
            :content (get-node-content node-el context))))))

;;

(defn get-allowed-tags-in-node [node-tag]
  (cond
    (= :body node-tag)
      tagdef/paragraph-tags
    (contains? tagdef/paragraph-tags node-tag)
      (set/union tagdef/block-tags tagdef/inline-tags)
    (contains? tagdef/block-tags node-tag)
      tagdef/inline-tags
    (contains? #{:cite :term :notation :def :f :b :i} node-tag)
      #{}
    (= :text node-tag)
      #{:cite :term :notation :def :f :b :i}
    (= :list node-tag)
      #{:item}
    (= :item node-tag)
      tagdef/inline-tags))

(defn get-allowed-children-of-node [node-el]
  (let [node-tag (:tag node-el)]
    (filter (fn [child-el]
              (or (string? child-el)
                  (contains? (get-allowed-tags-in-node node-tag)
                             (:tag child-el))))
            (zip/children (zip/xml-zip node-el)))))

(defn get-node-content [node-el context]
  (map #(node->map % context)
       (get-allowed-children-of-node node-el)))

(defn get-body-content [body-el context]
  (map-indexed
   (fn [idx node] (assoc (node->map node context)
                    :index (inc idx)))
   (get-allowed-children-of-node body-el)))

;;

(defn read-document->zipper [doc-name bun-meta]
  (-> (bp/get-doc-file doc-name (bun-meta :path))
      io/input-stream
      xml/parse
      zip/xml-zip))

(defn doc-zipper->map [doc-zipper doc-name bun-meta]
  (reset-formula-counter!)
  {:name        doc-name
   :title       (get-text-of-child-of-xml-element doc-zipper :title)
   :description (get-text-of-child-of-xml-element doc-zipper :description)
   :body        (get-body-content (zip/node (zip-xml/xml1-> doc-zipper :body))
                                  {:doc-name        doc-name
                                   :bun-meta        bun-meta})})

(defn read-document
  "Find the document with the given name in the given bundle, and read its
  contents as a map."
  [doc-name bun-meta]
  (doc-zipper->map (read-document->zipper doc-name bun-meta) doc-name bun-meta))

(defn list-paragraphs-in-document [doc-zipper]
  (map (fn [n] (:name (:attrs n))) (zip/children (zip-xml/xml1-> doc-zipper :body))))

(defn list-formulas-in-document [doc-zipper]
  (map (fn [n] (:name (:attrs n)))
     (filter (fn [n] (and (= (:tag n) :f)
                     (:name (:attrs n))))
             (map zip/node (zip-xml/xml-> doc-zipper :body data-zip/descendants)))))

;;;;;;;;

(defn- load-dep [dep-name bun-meta]
  (if (= dep-name (bun-meta :name))
    bun-meta
    (if-let [dep (get (bun-meta :deps-map) dep-name)]
      (if-let [dep-path (dep :path)]
        (bm/read-meta dep-path)
        (throw+ {:type ::bad-reference :subtype ::bundle-not-installed}))
      (throw+ {:type ::bad-reference :subtype ::bundle-not-loaded}))))

(defn- resolve-ref [ref doc-name bun-meta]
  "Resolve the index of the document in the bundle, and the index of the
  paragraph in the document."
  (if-let [resolved (try+
                     (let [dep-meta (load-dep (ref :bun-name) bun-meta)]
                       (when (bm/contains-document? dep-meta (ref :doc-name))
                        (let [dep-doc-idx  (inc (.indexOf (dep-meta :contents) (ref :doc-name)))
                              dep-par-idx  (if (ref :par-name)
                                             (inc (.indexOf (list-paragraphs-in-document
                                                             (read-document->zipper (ref :doc-name) dep-meta))
                                                            (ref :par-name)))
                                             0)
                              dep-fml-idx  (if (ref :fml-name)
                                             (inc (.indexOf (list-formulas-in-document
                                                             (read-document->zipper (ref :doc-name) dep-meta))
                                                            (ref :fml-name)))
                                             0)]
                          (cond
                           (not= 0 dep-par-idx) {:doc-index   dep-doc-idx
                                                 :par-index   dep-par-idx
                                                 :bun-author  (dep-meta :author)
                                                 :bun-version (dep-meta :version)}
                           (not= 0 dep-fml-idx) {:doc-index   dep-doc-idx
                                                 :fml-index   (integer->letter dep-fml-idx)
                                                 :bun-author  (dep-meta :author)
                                                 :bun-version (dep-meta :version)}
                           :else                (throw+ {:type ::bad-reference :subtype ::ref-not-found
                                                         :ref  ref})))))
                    (catch [:type ::bad-reference :subtype ::bundle-not-loaded] _
                      (println "Bad ref in document " (str doc-name ";")
                               "bundle not loaded:"
                               (str (ref :bun-name) "/" (ref :doc-name) "/" (ref :par-name))))
                    (catch [:type ::bad-reference :subtype ::bundle-not-installed] _
                      (println "Bad ref in document " (str doc-name ";")
                               "bundle not installed:"
                               (str (ref :bun-name) "/" (ref :doc-name) "/" (ref :par-name))))
                    (catch [:type ::bad-reference :subtype ::ref-not-found] _
                      (println (str "Bad ref in document " doc-name ":")
                                    (str (ref :bun-name) "/" (ref :doc-name) "/"
                                         (if (ref :par-name)
                                           (ref :par-name)
                                           (ref :fml-name))))))]
    (merge ref resolved)
    ref))

;(defn- resolve-ext-ref [ref doc-name bun-meta]
;  "Resolve the ref-index of the ext-ref."
;  (if-let [resolved ]
;    (merge ref resolved)
;    ref))

(defn- resolve-refs-in-node [node-map doc-name bun-meta]
  (if (string? node-map)
    node-map
    (if-let [ref (when (map? node-map) (node-map :ref))]
      (assoc node-map :ref (resolve-ref ref doc-name bun-meta))
      (if-let [fml-ref (when (map? node-map) (node-map :formula-ref))]
        (assoc node-map :formula-ref (resolve-ref fml-ref doc-name bun-meta))
        (if (string? (node-map :content))
          node-map
          (assoc node-map :content
            (map #(resolve-refs-in-node % doc-name bun-meta)
                 (node-map :content))))))))

(defn resolve-refs [doc-map doc-name bun-meta]
  (assoc doc-map :body
    (map #(resolve-refs-in-node % doc-name bun-meta)
         (doc-map :body))))

;(def bun1-path
;  (str "resources/sample"))
;(read-document "firstdoc" (bm/read-meta bun1-path))

;; {:title "The first document", :description " This is the first document in this bundle. ", :body ({:content ("\nConsider the following equation:\n  " {:content (" x^2 + y^2 = z^2 "), :type :formula} "\nAny triple of integers $(x,y,z)$ satisfying this equation is called a " {:ref nil, :ext-ref nil, :type :term} ".\n"), :name nil, :type :definition} {:content ({:content ("\n    A triple $(x, y, z)$ is " {:ref {:par-name "Pythagorean triple", :doc-name "firstdoc", :bun-name nil}, :ext-ref nil, :type :term} " if and only if $(y, x, z)$ is.\n  "), :type :statement} {:content ("\n    Follows from the definition.\n  "), :type :proof}), :name nil, :type :theorem} {:content ("\n  Note that the triple $(3, 4, 5)$ is " {:ref {:par-name "Pythagorean triple", :doc-name "firstdoc", :bun-name nil}, :ext-ref nil, :type :term} ".\n"), :name nil, :type :example} {:content ({:content ("\n    The triple $(4, 3, 5)$ is Pythagorean.\n  "), :type :statement} {:content ("\n    This follows from the theorem " " applied to the example " ".\n  "), :type :proof}), :name nil, :type :corollary})}
