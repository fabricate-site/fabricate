(ns site.fabricate.prototype.read.grammar
  (:require [instaparse.core :as insta]))

(def form
  (insta/parser
   "form = EPSILON | ( expr | #'([a-zA-Z]|\\s)*' | form)*
    expr = ('✳=' | '✳') #'[a-z]*' '🔚'"))

(comment
  (form "✳=abcd🔚 some text"))
