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

(def ^:private txt-insta-regex
  "the left side of txt's regex is a fast possessive quantifier for the easy case, the right side is the more complex lookahead"
  (let [fast-possessive "(\\A[^✳🔚]*+\\Z)"
        reluctant-txt "[\\S\\s]*?"
        terminal-lookahead "(?=\\Z|(?:[\\]})]//🔚|✳|🔚))"]
    (str fast-possessive "|(" reluctant-txt terminal-lookahead ")")))

(def template
  ;; ext-form-open = initial '//' open-form
  ;; ext-form-close = close-form '//' terminal
  ;; open-form = ( '[' | '(' | '{' ) <'\n'>
  ;; close-form = ']' | ')' | '}'
  ;; extended-form = ext-form-open ( expr | txt )* ext-form-close

  (insta/parser
   (format
    "template = EPSILON | ( expr | txt | extended-form )*
    initial = '✳'
    terminal = '🔚'
    expr = <initial> !'//'  ('=' | '+' | '+=')?  #'[^=+][^🔚]*' !'//' <terminal>

    (* the left side of txt's regex is a fast possessive quantifier
       for the easy case, the right side is the more complex lookahead *)
    txt = #'%s'

    (* extended forms allow arbitrary nesting without breaking the flow *)

    extended-form = (<initial> <'//'> '[' <'\n'> (expr|txt|extended-form)+ ']' <'//'> <terminal>) |
                    (<initial> <'//'> '(' <'\n'> (expr|txt|extended-form)+ ')' <'//'> <terminal>) |
                    (<initial> <'//'> '{' <'\n'> (expr|txt|extended-form)+ '}' <'//'> <terminal>)"
    txt-insta-regex)))
