(ns sableman.util
  (:gen-class)
  (:require [clojure.data.zip.xml :as zip-xml]))

(defn get-text-of-child-of-xml-element
  "A convenience function that checks if the given xml element has a child with
  the given tag name, and returns its text content if so, and the empty string otherwise.
  The element should be given in the form of an xml zipper."
  [element child-tag]
  (if-let [child (zip-xml/xml1-> element child-tag)]
    (zip-xml/text child)
    ""))
