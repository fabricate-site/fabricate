(ns site.fabricate.prototype.read.grammar
  (:require [instaparse.core :as insta]))

(def form
  (insta/parser
   "form = EPSILON | ( expr | #'(?:[^✳.*🔚]*)' | form)*
    expr = ('✳=' | '✳') #'(?:[^✳=?.*🔚]*)' '🔚'"))


(comment


  (re-matches  #"(?:[^✳.*🔚]*)"
               "abc")

  (re-matches  #"(?:[^✳.*🔚]*)"
               "✳abc🔚")

  (form "✳=abcd🔚 some text")

  (form "✳=(+ 3 4 5)🔚 some text")

  )
