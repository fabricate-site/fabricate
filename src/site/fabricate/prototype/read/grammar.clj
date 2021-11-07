(ns site.fabricate.prototype.read.grammar
  (:require [instaparse.core :as insta]))

(def form
  (insta/parser
   "form = EPSILON | ( expr | txt )*
    txt = #'(?:[^✳🔚]*)'
    expr = #'✳=?' #'[^🔚]*' '🔚'"))

(comment

  (re-matches  #"(?:[^✳🔚]*)"
               "abc")

  (re-matches  #"(?:[^✳🔚]*)"
               "✳abc🔚")

  (re-matches  #"(?:[^✳🔚]*)"
               "something.")

  (form "✳=abcd🔚 some text")

  (form "✳=(+ 3 4 5)🔚 some text")

  (form "✳=(my.ns/fn  22)🔚 some text"))
