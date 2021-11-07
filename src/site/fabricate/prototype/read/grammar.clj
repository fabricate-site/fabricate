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

  (form "✳=(my.ns/fn  22)🔚 some text")

  (form "a first sentence. another sentence.")

  (form "When I was on my 𝑛th refactor of a thread macro with multiple embedded if expressions, I realized I was trying to handle lots of special cases without really thinking about them. But in the back of my mind, I knew I was trying to dispatch my function calls on the basis of whether the data I was passing through my function matched some predicate or other - another way of saying that the data was in a particular ✳= [:em \"state.\"] 🔚")

  (form (slurp "./pages/finite-schema-machines.html.fab"))

  (take 3 (insta/parses form  "When I was on my 𝑛th refactor of a thread macro with multiple embedded if expressions, I realized I was trying to handle lots of special cases without really thinking about them. But in the back of my mind, I knew I was trying to dispatch my function calls on the basis of whether the data I was passing through my function matched some predicate or other - another way of saying that the data was in a particular ✳= [:em \"state.\"] 🔚"))

  )
