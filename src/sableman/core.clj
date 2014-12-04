(ns sableman.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.zip :as zip] ; these are for repl testing
            [clojure.data.zip.xml :as zip-xml]
            [clojure.data.xml :as xml]
            [clojure.tools.cli :refer [cli]])
  (:import [org.apache.commons.io FileUtils])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [sableman.bundle.path :as sabpat]
            [sableman.bundle.meta :as sabmet]
            [sableman.export.html :as sabexp]))

(defn- install-bundle! [bun-path]
  (let [bun-meta     (sabmet/read-meta bun-path)
        install-path (sabpat/get-installed-bundle-path bun-meta)]
    (FileUtils/copyDirectory bun-path install-path)
    (println "Installed the bundle\n  ->"
             (str (bun-meta :author) "/" (bun-meta :name))
             (str "@v" (bun-meta :version)))))

(def cli-options
  ["-h" "--help" "Show this help"])

(defn- parse-path-and-meta [args]
  (case (count args)
    1 [(first args)
       (sabmet/read-meta (first args))]
    3 (let [bun-meta {:author  (nth args 0)
                      :name    (nth args 1)
                      :version (nth args 2)}
            bun-path (sabpat/get-installed-bundle-path bun-meta)]
        [bun-path bun-meta])
      (do (println "Error: please specify either a path"
                   "or a triple (author, name, version).")
        (shutdown-agents))))

(defn -main [& args]
  (let [[opts args banner] (cli args cli-options)
        cmd                (first args)
        args               (rest args)]
    (cond
     (or (not cmd) (:help opts))
       (println banner)
     (= "export" cmd)
       (let [[bun-path bun-meta] (parse-path-and-meta args)]
         (if (and bun-path (sabpat/is-bundle? bun-path))
           (try+
            (sabexp/export-bundle-to-html! bun-path)
            (println "Exported bundle to path:\n" (str (sabpat/get-html-dir bun-meta)))
            (shutdown-agents)
            (catch [:type :sableman.export.html/missing-deps] {:keys [missing-deps]}
              (println "Error: the following dependencies need to be installed first:\n"
                       missing-deps)))
           (println "Error: there is no such bundle installed.")))
     (= "install" cmd)
       (let [bun-path (io/file (first args))]
         (if (and bun-path (sabpat/is-bundle? bun-path))
           (do (install-bundle! bun-path)
               (shutdown-agents))
           (println (str "The path '" bun-path "' is not a valid bundle."))))
     :else
       (println (str "'" cmd "' is not a sableman command")))))

; (defn read-bundle [uri-str]
;   (let [bundle-location (bun-loc/parse-uri-str uri-str)]
;     (if (bun-loc/is-valid? bundle-location)
;       (bun-meta/read-meta bundle-location)
;       (throw+ {:type ::bad-bundle-location
;                :bundle-location bundle-location}))))

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
