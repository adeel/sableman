(ns sableman.bundle.document
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
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

;; parsing functions

;; parsing strategy:
; need a function that, given :paragraph or :formula or :proof,
; returns the subset of tags that are allowed within that tag
; one can have a generic parse-node function which parses only
; the children tags which are in this subset

(defn get-allowed-tags-in-node [node-tag]
  (cond
    (= :body node-tag)
      tagdef/paragraph-tags
    (contains? tagdef/paragraph-tags node-tag)
      (set/union tagdef/block-tags tagdef/inline-tags)
    (contains? tagdef/block-tags node-tag)
      tagdef/inline-tags
    (contains? #{:cite :term :notation :def :f} node-tag)
      #{}
    (= :text node-tag)
      #{:cite :term :notation :def :f}
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

(declare get-node-content)

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

(defn linked-node->map
  "Expects one of the following:
    * ext-ref='someexternalreference'
    * ref='some paragraph in this document'
    * ref='anotherdocinthisbundle/a paragraph in that document'
    * ref='anotherbundle/somedoc/some paragraph in that doc
  plus optionally :pre-note and/or :post-note."
  [node-el doc-name bun-meta]
  (let [node-zipper  (zip/xml-zip node-el)
        ref-str      (zip-xml/attr node-zipper :ref)
        ref-map      (when ref-str (ref-str->map ref-str doc-name (bun-meta :name)))
        ext-ref      (zip-xml/attr node-zipper :ext-ref)
        pre-note     (zip-xml/attr node-zipper :pre-note)
        post-note    (zip-xml/attr node-zipper :post-note)]
    {:type      (:tag node-el)
     :ref       ref-map
     :ext-ref   ext-ref
     :pre-note  pre-note
     :post-note post-note
     :content   (get-node-content node-el doc-name bun-meta)}))

(defn def-node->map [node-el doc-name bun-meta]
  {:type    :def
   :content (get-node-content node-el doc-name bun-meta)})

(defn formula-node->map [node-el doc-name bun-meta]
  {:type    :formula
   :content (get-node-content node-el doc-name bun-meta)})

(defn list-node->map [node-el doc-name bun-meta]
  (let [node-zipper (zip/xml-zip node-el)
        list-type   (zip-xml/attr node-zipper :type)]
    {:type      :list
     :content   (get-node-content node-el doc-name bun-meta)
     :list-type (or list-type "disc")}))

(defn list-item-node->map [node-el doc-name bun-meta]
  {:type    :list-item
   :content (get-node-content node-el doc-name bun-meta)})

(defn node->map [node-el doc-name bun-meta]
  (if (string? node-el)
    node-el
    (let [node-tag     (:tag node-el)
          node-zipper  (zip/xml-zip node-el)
          node-map     {:type node-tag}]
      (cond
        (contains? tagdef/paragraph-tags node-tag)
          (merge node-map
            {:name   (zip-xml/attr node-zipper :name)
             :content (get-node-content node-el doc-name bun-meta)})
        (= :cite node-tag)
          (assoc (merge node-map (linked-node->map node-el doc-name bun-meta))
            :type :citation)
        (= :notation node-tag)
          (merge node-map (linked-node->map node-el doc-name bun-meta))
        (= :term node-tag)
          (merge node-map (linked-node->map node-el doc-name bun-meta))
        (= :def node-tag)
           (merge node-map (def-node->map node-el doc-name bun-meta))
        (= :f node-tag)
           (merge node-map (formula-node->map node-el doc-name bun-meta))
        (= :list node-tag)
           (merge node-map (list-node->map node-el doc-name bun-meta))
        (= :list-item node-tag)
           (merge node-map (list-item-node->map node-el doc-name bun-meta))
        :else
          (assoc node-map
            :content (get-node-content node-el doc-name bun-meta))))))

(defn get-node-content [node-el doc-name bun-meta]
  (map #(node->map % doc-name bun-meta)
       (get-allowed-children-of-node node-el)))

(defn get-body-content [body-el doc-name bun-meta]
  (map-indexed
   (fn [idx node] (assoc (node->map node doc-name bun-meta)
                    :index (inc idx)))
   (get-allowed-children-of-node body-el)))

(defn read-document
  "Find the document with the given name in the given bundle, and read its
  contents as a map."
  [doc-name bun-meta]
  (let [doc-zipper (-> (bp/get-doc-file doc-name (bun-meta :path))
                        io/input-stream
                        xml/parse
                        zip/xml-zip)
        body       (get-body-content (zip/node (zip-xml/xml1-> doc-zipper :body))
                                     doc-name bun-meta)]
    {:name        doc-name
     :title       (get-text-of-child-of-xml-element doc-zipper :title)
     :description (get-text-of-child-of-xml-element doc-zipper :description)
     :body        body}))

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
                       (if (bm/contains-document? dep-meta (ref :doc-name))
                        (let [dep-doc-idx (inc (.indexOf (dep-meta :contents) (ref :doc-name)))
                              dep-doc (read-document (ref :doc-name) dep-meta)
                              dep-doc-pars (map #(get % :name) (dep-doc :body))
                              dep-par-idx  (inc (.indexOf dep-doc-pars (ref :par-name)))]
                          (when (= 0 dep-par-idx)
                            (println "Bad ref in document " (str doc-name ";")
                                     "paragraph not found:"
                                     (str (ref :bun-name) "/" (ref :doc-name) "/" (ref :par-name))))
                          {:doc-index   dep-doc-idx
                           :par-index   dep-par-idx
                           :bun-author  (dep-meta :author)
                           :bun-version (dep-meta :version)})
                        (println "Bad ref in document " (str doc-name ";")
                                 "document not found:"
                                 (str (ref :bun-name) "/" (ref :doc-name) "/" (ref :par-name)))))
                    (catch [:type ::bad-reference :subtype ::bundle-not-loaded] _
                      (println "Bad ref in document " (str doc-name ";")
                               "bundle not loaded:"
                               (str (ref :bun-name) "/" (ref :doc-name) "/" (ref :par-name))))
                    (catch [:type ::bad-reference :subtype ::bundle-not-installed] _
                      (println "Bad ref in document " (str doc-name ";")
                               "bundle not installed:"
                               (str (ref :bun-name) "/" (ref :doc-name) "/" (ref :par-name)))))]
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
      (if (string? (node-map :content))
        node-map
        (assoc node-map :content
          (map #(resolve-refs-in-node % doc-name bun-meta)
               (node-map :content)))))))

(defn resolve-refs [doc-map doc-name bun-meta]
  (assoc doc-map :body
    (map #(resolve-refs-in-node % doc-name bun-meta)
         (doc-map :body))))

;(def bun1-path
;  (str "resources/sample"))
;(read-document "firstdoc" (bm/read-meta bun1-path))

;; {:title "The first document", :description " This is the first document in this bundle. ", :body ({:content ("\nConsider the following equation:\n  " {:content (" x^2 + y^2 = z^2 "), :type :formula} "\nAny triple of integers $(x,y,z)$ satisfying this equation is called a " {:ref nil, :ext-ref nil, :type :term} ".\n"), :name nil, :type :definition} {:content ({:content ("\n    A triple $(x, y, z)$ is " {:ref {:par-name "Pythagorean triple", :doc-name "firstdoc", :bun-name nil}, :ext-ref nil, :type :term} " if and only if $(y, x, z)$ is.\n  "), :type :statement} {:content ("\n    Follows from the definition.\n  "), :type :proof}), :name nil, :type :theorem} {:content ("\n  Note that the triple $(3, 4, 5)$ is " {:ref {:par-name "Pythagorean triple", :doc-name "firstdoc", :bun-name nil}, :ext-ref nil, :type :term} ".\n"), :name nil, :type :example} {:content ({:content ("\n    The triple $(4, 3, 5)$ is Pythagorean.\n  "), :type :statement} {:content ("\n    This follows from the theorem " " applied to the example " ".\n  "), :type :proof}), :name nil, :type :corollary})}
