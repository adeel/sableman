(ns sableman.bundle.meta
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.data.xml :as xml])
  (:use [slingshot.slingshot :only [throw+]])
  (:require [sableman.bundle.path :as bp]
            [sableman.bundle.refs :as br])
  (:use [sableman.util]))

;; stuff related to the metafile of the bundle

;(defn- parse-metafile [element]
;    (if (string? element)
;      element
;      (hash-map
;       (keyword (:tag element))
;       (merge (:attrs element)
;         {:content
;           (if (not= (count (:content element)) 1)
;             (map parse-metafile (:content element))
;             (parse-metafile (first (:content element))))}))))

(defn read-meta
  "Reads the metafile of the given bundle into a map."
  [bun-path]

  (let [meta-zipper  (-> (bp/get-metafile bun-path)
                       io/file
                       io/input-stream
                       xml/parse
                       zip/xml-zip)
        deps-vec     (if-let [deps-els (zip-xml/xml-> meta-zipper
                                        :dependencies :dependency)]
                       (map (fn [dep-el]
                              (let [dep-map (-> dep-el zip/node :attrs)]
                                (assoc dep-map
                                  :as (get dep-map :as (get dep-map :name)))))
                            deps-els) ; TODO: select keys :name, :author, :as
                       ())
        deps-map     (into {}
                           (map (fn [dep-map] [(dep-map :as) dep-map])
                                deps-vec))
        contents     (if-let [doc-els (zip-xml/xml-> meta-zipper :contents :document)]
                       (remove string/blank?
                         (map #(zip-xml/attr % :name) doc-els))
                       ())
        tex-preamble (if-let [zipper (zip-xml/xml1-> meta-zipper :tex-preamble)]
                       (apply str (:content (zip/node zipper))))]
    {:path         bun-path
     :author       (get-text-of-child-of-xml-element meta-zipper :author)
     :name         (get-text-of-child-of-xml-element meta-zipper :name)
     :title        (get-text-of-child-of-xml-element meta-zipper :title)
     :description  (get-text-of-child-of-xml-element meta-zipper :description)
     :version      (get-text-of-child-of-xml-element meta-zipper :version)
     :deps-vec     deps-vec
     :deps-map     deps-map
     :contents     contents
     :tex-preamble tex-preamble}))

;;;;;;;

(defn load-titles [bun-meta]
  (assoc bun-meta :titles
    (map-indexed
     (fn [i doc-name]
       (let [doc-zipper (-> (bp/get-doc-file doc-name (bun-meta :path))
                            io/input-stream
                            xml/parse
                            zip/xml-zip)]
         [(inc i)
          doc-name
          (get-text-of-child-of-xml-element doc-zipper :title)]))
     (bun-meta :contents))))

;;;;;;;

(defn find-bundle-in-user-paths [bun-meta]
  (let [path (bp/get-installed-bundle-path bun-meta)]
    (if (bp/is-bundle? path)
      path
      (first
       (map (fn [p] (first
                     (filter (fn [bun-path]
                               (if-let [m (read-meta bun-path)]
                                 (= [(bun-meta :author)
                                     (bun-meta :name)
                                     (bun-meta :version)]
                                    [(m :author)
                                     (m :name)
                                     (m :version)])))
                            (filter bp/is-bundle? (file-seq p)))))
            bp/user-paths)))))

(defn load-deps [bun-meta]
  (assoc bun-meta :deps-map
    (into {}
          (map (fn [[k v]]
                 [k (assoc v :path (find-bundle-in-user-paths v))])
          (bun-meta :deps-map)))))

;;;;;;;;

(defn load-ext-refs [bun-path bun-meta]
  (assoc bun-meta :ext-refs (br/read-refs bun-path)))

;;;;;;;;

;(defn resolve-dependency [dep]
;  (when (string/blank? (dep :name))
;    (throw-exception! ::bad-dependency-no-name
;      bundle-location bundle-meta {:dependency dep}))
;  (when (string/blank? (dep :author))
;    (throw-exception! ::bad-dependency-no-author
;      bundle-location bundle-meta {:dependency dep}))
;  (let [dep-location (bun-loc/parse-uri-str (dep :uri))]
;    (when-not (bun-loc/is-valid? dep-location)
;      (throw-exception! ::bad-dependency-invalid-location
;        bundle-location bundle-meta {:dependency dep}))
;    {:location dep-location
;     :meta     (read-meta dep-location)}))

;(defn load-dependencies
;  [bundle-meta]
;  (assoc bundle-meta :dependencies-map
;    (into {}
;      (map
;        (fn [dep] [(get-in dep [:meta :name]) dep])
;        (map resolve-dependency (bundle-meta :dependencies))))))

(defn contains-document?
  "Returns true if the list of contents in the metafile includes a document of
  the given name."
  [bun-meta doc-name]
  (some #{doc-name} (bun-meta :contents)))

(defn- throw-exception!
  ([subtype bun-meta]
    (throw+ {:type ::bad-metafile :subtype subtype :bun-meta bun-meta}))
  ([subtype bun-meta more]
    (throw+
     (merge {:type ::bad-metafile :subtype subtype :bun-meta bun-meta}
            more))))

(defn validate-dependencies! [bun-meta]
  (doseq [dep (bun-meta :deps-vec)]
    (when (string/blank? (dep :name))
      (throw-exception! ::bad-dependency-no-name bun-meta {:dependency dep}))
    (when (string/blank? (dep :author))
      (throw-exception! ::bad-dependency-no-author bun-meta {:dependency dep}))
    (when (string/blank? (dep :as))
      (throw-exception! ::bad-dependency-no-as bun-meta {:dependency dep}))
    (when (and (= (dep :author) (bun-meta :author))
               (= (dep :name)   (bun-meta :name)))
      (throw-exception! ::bad-dependency-self-reference bun-meta
                        {:dependency dep})))
  (letfn [(distinct? [vec] ; built-in distinct? seems broken...
            (= vec (distinct vec)))]
    (when-not (distinct? (map :as (bun-meta :deps-vec)))
      (throw-exception! ::bad-dependency-duplicate bun-meta))))

(defn validate-contents! [bun-meta]
  (doseq [doc-name (bun-meta :contents)]
    (when-not (bp/document-exists? doc-name (bun-meta :path))
      (throw-exception! ::bad-contents-document-not-found bun-meta
                        {:doc-name doc-name}))))

(defn validate!
  "Perform the following validations on the given bundle:
    * author: must not be blank
    * name: must not be blank
    * title: must not be blank
    * dependencies: each dependency must have a uri attribute which points to
      a valid bundle
    * contents: each listed document must be present in the bundle.
  Throws an exception of type ::bad-metafile in case of a validation error."
  [bun-meta]

  (when (string/blank? (bun-meta :author))
    (throw-exception! ::no-author bun-meta))
  (when (string/blank? (bun-meta :name))
    (throw-exception! ::no-name bun-meta))
  (when (string/blank? (bun-meta :title))
    (throw-exception! ::no-title bun-meta))
  (validate-dependencies! bun-meta)
  (validate-contents! bun-meta))
