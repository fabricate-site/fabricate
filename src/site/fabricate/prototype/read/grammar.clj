(ns site.fabricate.prototype.read.grammar
  (:require [instaparse.core :as insta]))

(def template
  (insta/parser
   "template = EPSILON | ( expr | txt | extended-form )*
    expr = '✳' #'=?[^🔚]*' '🔚'
    txt = #'[^✳]*'
    form-open = '✳//'
    form-close = '//🔚'
    extended-form = form-open ( expr | txt )* form-close "))

(comment

  (re-matches #"//" "//")

  (template "✳// text more text //🔚 ✳nil🔚")

  (template "text (with parens)")

  (template "✳// text more text //🔚 ✳(+ 3 4)🔚")

  (template "✳// text, followed by expr ✳(+ 3 4)🔚 and text //🔚 ✳(+ 3 4)🔚")

  (insta/parse template "some/text" :trace true)



  )
