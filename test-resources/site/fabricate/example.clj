(ns site.fabricate.example
  "Example namespace for testing parsing and evaluation of clojure forms"
  (:require [clojure.string :as str]
            [scicloj.kindly.v4.kind :as kind]))

;; This file demonstrates behavior expected of Fabricate's initial clojure
;; parsing function. This particular multi-line comment block should be
;; combined into a single element after parsing.

(map #(* 2 (+ 1 %)) [1 3 5 7 9 11]) ;; this should have code and results

'(quoted-form a b c d) ;; this quoted form should be displayed as-is


^:kindly/hide-code '(hidden-form "example") ;; this quoted form should be hidden

;; This is one paragraph of a comment.

;; And this is a second paragraph of a comment, because of the 2+ linebreaks.

;; This is the third paragraph. No clojure forms between - one single HTML
;; <div> element, with 3 <p> elements within.

(throw (ex-info "A test error" {:context "testing" :severity :trivial}))

;; the above error should be surfaced, but not block subsequent forms.
;; (unless "strict" mode is enabled)

(kind/fragment (kind/hiccup [:div "fragment 1"]) (kind/md "fragment 2"))

;; macro example
(defmacro unless [pred a b] `(if (not ~pred) ~a ~b))

;; TODO: add:
; - reader conditional
; - syntax quote

; from Janet Carr's blog post:
; https://blog.janetacarr.com/a-primer-on-clojure-macros/

(def x 5)
(def lst '(a b c))
`(fred x ~x lst ~@lst 7 8 :nine)

#?(:clj (kind/hiccup [:div "fragment inside reader conditionals"]))

;; tagged literals
(let [timestamp #inst "2025-01-01T00:00:00.000-00:00"]
  [:div "happy new year - the time is " (str timestamp)])

;; read-eval

#=(mapv inc [3 8 2 5])
