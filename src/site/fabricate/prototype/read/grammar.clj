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
         (re-seq #"^.*?(?=(?:🔚|//🔚)|$)" "text with ending//🔚")

         (re-seq #"^[^\s\S✳]*?(?=(?:🔚)|$)" "text with embedded ✳ and more txt")

         (re-seq #"\A[^✳🔚]++|([\S\s]*?(?=(?:✳|/{2}?🔚)|\Z))"
                 "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and")


         ((insta/parser "r = EPSILON
w = #'\\s?$'") "\n")

         (re-seq #"\s$" "\n")

         (insta/parse
          (insta/parser "rule = ( text | terminal | EPSILON ) *
                         text = #'^[\\s\\S]*?(?=(?:/{2}?🔚)|$)$'
                         terminal = <#'/{2}?🔚'>")
          "text with ending\n"
          :trace true)

         )

(def template
  ;; ext-form-open = initial '//' open-form
  ;; ext-form-close = close-form '//' terminal
  ;; open-form = ( '[' | '(' | '{' ) <'\n'>
  ;; close-form = ']' | ')' | '}'
  ;; extended-form = ext-form-open ( expr | txt )* ext-form-close

  (insta/parser
   "template = EPSILON | ( expr | txt )*
    initial = '✳'
    terminal = '🔚'
    expr = <initial> !'//' #'(=|\\+)?[^🔚]*' <terminal>
    txt = #'(\\A[^✳🔚]*+)|([\\S\\s]*?(?=\\Z|(?:✳|/{2}?🔚)))'"))

(comment

  (re-matches #".*(?=↩)" "something ↩")

  (re-matches #"//" "//")

  (template "✳//[:div
 text more text ]//🔚 ✳nil🔚")

  (template "text (with parens)")

  (template "✳// text more text //🔚 ✳(+ 3 4)🔚")

  (template "text, followed by expr ✳(+ 3 4)🔚 and text  ✳(+ 3 4)🔚")

  (insta/parse template "some/text" :trace true)


  (insta/parse
   template "↪[
 text]↩ ✳nil🔚"
   :total true)

  )
