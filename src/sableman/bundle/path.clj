(ns sableman.bundle.path
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.zip :as zip] ; these are for repl testing
            [clojure.data.zip.xml :as zip-xml]
            [clojure.data.xml :as xml])
  (:import [java.net URI])
  (:use [slingshot.slingshot :only [throw+]]))
  ; (:require [sableman.bundle-location :as bun-loc]
  ;           [sableman.bundle-meta :as bun-meta]
  ;           [sableman.bundle-documents :as bun-doc]))


;; user-specific

(def home-dir (System/getProperty "user.home"))

(def sableman-dir
  (io/file (or (System/getenv "SAB_DIR")
               (str home-dir "/.sableman"))))
(.mkdir sableman-dir)

(def bundle-repo-dir
  (io/file (str sableman-dir "/bundles")))
(.mkdir bundle-repo-dir)

(def user-path-str
  (or (System/getenv "SAB_PATH") ""))

(def user-paths
  (map io/file (string/split user-path-str #";")))

(def export-dir
  (io/file (or (System/getenv "SAB_EXPORT_DIR")
               (str sableman-dir "/export"))))
(.mkdir export-dir)

(def html-export-dir
  (io/file (or (System/getenv "SAB_HTML_EXPORT_DIR")
               (str export-dir "/html"))))
(.mkdir html-export-dir)

(def formula-cache-dir
  (io/file (str html-export-dir "/.formulas")))
(.mkdir formula-cache-dir)

(def tex-export-dir
  (io/file (or (System/getenv "SAB_TEX_EXPORT_DIR")
               (str export-dir "/tex"))))
(.mkdir tex-export-dir)

;; bundle-specific

(defn get-metafile [bun-path]
  (io/file (str bun-path "/" "bundle.sbl")))

(defn get-doc-file [doc-name bun-path]
  (io/file (str bun-path "/docs/" doc-name ".sbl")))

(defn get-reffile [bun-path]
  (io/file (str bun-path "/" "refs.sbl")))

;;

(defn get-tex-dir [bun-path]
  (io/file (str bun-path "/tex")))

(defn get-tex-file [bun-path bun-meta]
  (.mkdir (io/file (get-tex-dir bun-path)))
  (io/file (str (get-tex-dir bun-path) "/" (bun-meta :name) ".tex")))

;;

(defn get-html-dir [bun-meta]
  (.mkdir (io/file
           (str html-export-dir
           "/" (bun-meta :author))))
  (.mkdir (io/file
           (str html-export-dir
           "/" (bun-meta :author)
           "/" (bun-meta :name))))
  (.mkdir (io/file
           (str html-export-dir
           "/" (bun-meta :author)
           "/" (bun-meta :name)
           "/" (bun-meta :version))))
  (str html-export-dir
       "/" (bun-meta :author)
       "/" (bun-meta :name)
       "/" (bun-meta :version)))

(defn get-html-file [doc-name bun-meta]
  (.mkdir (io/file (get-html-dir bun-meta)))
  (io/file (str (get-html-dir bun-meta) "/" doc-name ".html")))

(defn get-html-index [bun-meta]
  (.mkdir (io/file (get-html-dir bun-meta)))
  (io/file (str (get-html-dir bun-meta) "/index.html")))

(defn get-html-refs [bun-meta]
  (.mkdir (io/file (get-html-dir bun-meta)))
  (io/file (str (get-html-dir bun-meta) "/refs.html")))

(defn get-html-css [bun-meta]
  (.mkdir (io/file (get-html-dir bun-meta)))
  (io/file (str (get-html-dir bun-meta) "/style.css")))

(defn is-bundle? [bun-path]
  (and (.isDirectory (io/file bun-path))
       (.exists (get-metafile bun-path))))

(defn document-exists?
  "Returns true if there exists in the bundle a file at the same path where
  a document of the given name would be."
  [doc-name bun-path]
  (.exists (get-doc-file doc-name bun-path)))

(defn intro-exists? [bun-path]
  (document-exists? "intro" bun-path))

;; installed bundles repository

(defn get-bundle-metastr [bun-meta]
  (str (bun-meta :author) "/"
       (bun-meta :name) "/"
       (bun-meta :version)))

(defn get-installed-bundle-path [bun-meta]
  (let [d1 (str bundle-repo-dir "/" (bun-meta :author))
        d2 (str d1 "/" (bun-meta :name))
        d3 (str d2 "/" (bun-meta :version))]
    (.mkdir (io/file d1))
    (.mkdir (io/file d2))
    (.mkdir (io/file d3))
    (io/file d3)))

(defn is-installed? [bun-meta]
  (is-bundle? (get-installed-bundle-path bun-meta)))

; (defn read-bundle [uri-str]
;   (let [bundle-location (bun-loc/parse-uri-str uri-str)]
;     (if (bun-loc/is-valid? bundle-location)
;       (bun-meta/read-meta bundle-location)
;       (throw+ {:type ::bad-bundle-location
;                :bundle-location bundle-location}))))

; ;(defn export-bundle
; ;  "Export a bundle as :html or :pdf."
; ;  [bundle-uri frmt]
; ;  ; ...
; ;)

; ; (def make-bundle-map
; ;   "Makes a bundle map, given location and meta, by resolving the dependencies,
; ;   etc."
; ;   [bundle-location bundle-meta]
; ;   {:location bundle-location
; ;    :meta     (assoc bundle-meta :dependencies
; ;                (map bun-meta/resolve-dependency (bundle-meta :dependencies)))})

; (def bundle-location (bun-loc/uri-str->map "file:///mtvhtp"))
; (def metafile (bun-loc/get-metafile bundle-location))
; (def bundle-meta (bun-meta/read bundle-location))
; ; (bun-meta/validate! bundle-location bundle-meta)
; (def doc-name "mtvspc")
; (def bundle {:location bundle-location
;              :meta     (load-dependencies bundle-meta)})

; (def doc-zipper (-> (bun-loc/get-document doc-name (:location bundle))
;                         io/file
;                         io/input-stream
;                         xml/parse
;                         zip/xml-zip))
; (def pars (zip/children (zip-xml/xml1-> doc-zipper :body)))
; (def par (second pars))

; ; (def d (bun-doc/read "mtvspc" {:location bundle-location
; ;                                :meta bundle-meta}))

; (defn -main
;   "I don't do a whole lot ... yet."
;   [& args]
;   (bun-meta/validate! bundle-location bundle-meta))
