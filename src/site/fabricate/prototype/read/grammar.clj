(ns site.fabricate.prototype.read.grammar
  (:require [instaparse.core :as insta]))

(def template
  (insta/parser
   "template = EPSILON | ( expr | txt )*
    txt = #'(?:[^✳🔚]*)'
    expr = #'✳=?' #'[^🔚]*' '🔚'"))

(comment


  (re-matches  #"(?:[^✳🔚]*)"
               "abc")

  (re-matches  #"(?:[^✳🔚]*)"
               "✳abc🔚")

  (re-matches  #"(?:[^✳🔚]*)"
               "something.")

  (template "✳=abcd🔚 some text")

  (template "✳=(+ 3 4 5)🔚 some text")

  (template "✳=(my.ns/fn  22)🔚 some text")

  (let [post (slurp "./pages/finite-schema-machines.html.fab")
        post-with-meta (insta/add-line-and-column-info-to-metadata
     post
     (template post))]
    (meta (last post-with-meta)))


  )
