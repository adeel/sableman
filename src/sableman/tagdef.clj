(ns sableman.tagdef
  (:gen-class))

;; tag definitions

(def paragraph-tags
  #{:paragraph
    :definition
    :theorem
    :proposition
    :lemma
    :corollary
    :example
    :remark
    :text})

(def block-tags
  #{:statement
    :proof
    :reference
    :text})

(def inline-tags
  #{:cite
    :term
    :notation
    :def
    :f
    :list
    :text})
