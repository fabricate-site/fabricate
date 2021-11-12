(ns site.fabricate.prototype.read.grammar
  (:require [instaparse.core :as insta]))

(comment (require '[lambdaisland.regal :as regal])

         ;; the general idea here is :
         ;; if the terminal is present, match all text before it
         ;; (match without capturing)
         ;; if it is not present, match all the text

         ;; same regex matching on both, no char included
         (re-seq #"^.*?(?=🔚|$)" "text with ending🔚")

         (re-seq #"^.*?(?=🔚|$)" "text ")

         ;; reluctant quantifiers are the key here

         (re-seq #"^.*?(?=(?:🔚)|$)" "text with ending🔚")
         (re-seq #"^.*?(?=(?:/{2}?🔚)|$)" "text with ending//🔚")

         (re-seq #"^.*?(?=(?:🔚)|$)" "text ")
         (re-seq #"^.*?(?=(?:/?/?🔚)|$)" "text ")

         (insta/parse
          (insta/parser "rule = ( text | terminal) *
                         text = #'^.*?(?=🔚|$)'
                         terminal = '🔚'")
          "text with ending🔚")

         )

(def template
  ;; ext-form-open = initial '//' open-form
  ;; ext-form-close = close-form '//' terminal
  ;; open-form = ( '[' | '(' | '{' ) <'\n'>
  ;; close-form = ']' | ')' | '}'
  ;; extended-form = ext-form-open ( expr | txt )* ext-form-close

  (insta/parser
   "template = EPSILON | ( expr | txt | s )*
    initial = '✳'
    terminal = '🔚'
    expr = <initial> !'//' #'(=|\\+)?[^🔚]*' <terminal>
    <s> = <#'\\s+'>
    txt = #'[\\S\\s]*?(?=\\Z|(?:✳|/{2}?🔚))'"))

(comment

  (re-matches #"//" "//")

  (template "✳// text more text //🔚 ✳nil🔚")

  (template "text (with parens)")

  (template "✳// text more text //🔚 ✳(+ 3 4)🔚")

  (template "✳// text, followed by expr ✳(+ 3 4)🔚 and text //🔚 ✳(+ 3 4)🔚")

  (insta/parse template "some/text" :trace true)



  )
