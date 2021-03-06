(ns site.fabricate.prototype.read.grammar
  "Instaparse grammar for Fabricate's page templates."
  (:require [instaparse.core :as insta]))

(comment (require '[lambdaisland.regal :as regal])

         ;; the general idea here is :
         ;; if the terminal is present, match all text before it
         ;; (match without capturing)
         ;; if it is not present, match all the text

         ;; same regex matching on both, no char included
         (re-seq #"^.*?(?=π|$)" "text with endingπ")

         (re-seq #"^.*?(?=π|$)" "text ")

         ;; reluctant quantifiers are the key here

         (re-seq #"^.*?(?=(?:π)|$)" "text with endingπ")
         (re-seq #"^.*?(?=(?:π|//π)|$)" "text with ending//π")

         (re-seq #"^[^\s\Sβ³]*?(?=(?:π)|$)" "text with embedded β³ and more txt")

         (re-seq #"\A[^β³π]++|([\S\s]*?(?=(?:β³|/{2}?π)|\Z))"
                 "text (with parens) and an expr β³=(+ 3 4 5)π and")

         ((insta/parser "r = EPSILON
                         w = #'\\s?$'") "\n")
         (re-seq #"\s$" "\n")

         (insta/parse
          (insta/parser "rule = ( text | terminal | EPSILON ) *
                         text = #'^[\\s\\S]*?(?=(?:/{2}?π)|$)$'
                         terminal = <#'/{2}?π'>")
          "text with ending\n"
          :trace true))

(def ^:private txt-insta-regex
  "the left side of txt's regex is a fast possessive quantifier for the easy case, the right side is the more complex lookahead"
  (let [fast-possessive "(\\A[^β³π]*+\\Z)"
        reluctant-txt "[\\S\\s]*?"
        terminal-lookahead "(?=\\Z|(?:[\\]})]//π|β³|π))"]
    (str fast-possessive "|(" reluctant-txt terminal-lookahead ")")))

(def delimiters ["β³" "π"])

(def template
  "The formal grammar for Fabricate templates."
  (insta/parser
   (format
    "template = EPSILON | ( expr | txt | extended-form )*
    initial = 'β³'
    terminal = 'π'
    ctrl = ('=' | '+' | '+=')
    expr = <initial> !'//'  ctrl?  #'[^=+][^π]*' !'//' <terminal>

    (* the left side of txt's regex is a fast possessive quantifier
       for the easy case, the right side is the more complex lookahead *)
    txt = #'%s'

    (* extended forms allow arbitrary nesting without breaking the flow *)
    form-contents = (expr|txt|extended-form)+
    extended-form = (<initial> <'//'> '[' #'[^\nβ³π]*' <'\n'> form-contents ']' <'//'> <terminal>) |
                    (<initial> <'//'> '(' #'[^\nβ³π]*' <'\n'> form-contents ')' <'//'> <terminal>) |
                    (<initial> <'//'> '{' #'[^\nβ³π]*' <'\n'> form-contents '}' <'//'> <terminal>)"
    txt-insta-regex)))
