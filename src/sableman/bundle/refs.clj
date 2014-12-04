(ns sableman.bundle.refs
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.data.xml :as xml])
  (:use [slingshot.slingshot :only [throw+]])
  (:require [sableman.bundle.path :as bp])
  (:use [sableman.util]))

;; stuff related to the reffile of the bundle

(defn read-refs
  "Reads the metafile of the given bundle into a map."
  [bun-path]

  (let [refs-file (bp/get-reffile bun-path)]
    (if (.exists refs-file)
      (let [refs-zipper  (-> (bp/get-reffile bun-path)
                             io/input-stream
                             xml/parse
                             zip/xml-zip)
            refs-els     (filter #(= :ref (:tag %)) (zip/children refs-zipper))
            refs-map     (if refs-els
                           (into (sorted-map)
                             (map (fn [ref-el]
                                    [(get-in ref-el [:attrs :id] "id")
                                     (into {}
                                       (map (fn [attr-el]
                                              [(:tag attr-el)
                                               (string/join (:content attr-el))])
                                            (zip/children (zip/xml-zip ref-el))))])
                                  refs-els))
                           ())]
        refs-map)
      {})))

(read-refs "/home/adeel/Dropbox/Math/notes/sheaves")
