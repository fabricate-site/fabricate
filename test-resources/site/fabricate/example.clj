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
