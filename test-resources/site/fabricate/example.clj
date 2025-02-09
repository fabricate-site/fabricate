^{:kindly/hide-code true :kindly/hide-result true}
(ns ^{:site.fabricate.document/title "Fabricate Clojure example namespace"}
    site.fabricate.example
  "Example namespace for testing parsing and evaluation of clojure forms"
  (:require [clojure.string :as str]
            [scicloj.kindly.v4.kind :as kind]))

^{:kindly/kind :kind/hiccup}
(let [curr-ns *ns*
      ns-meta (meta curr-ns)]
  [:h1 (:site.fabricate.document/title ns-meta)])

;; This file demonstrates behavior expected of Fabricate's initial clojure
;; parsing function. This particular multi-line comment block should be
;; combined into a single element after parsing.

(map #(* 2 (+ 1 %)) [1 3 5 7 9 11]) ;; this should have code and results

;; arbitrary Hiccup data can be rendered to HTML by using :kind/hiccup

^{:kindly/kind :kind/hiccup} [:h2 "Other kindly options"]

;; right now only hide-code and hide-result are supported.

^:kindly/hide-code '(hidden-form "example") ;; this quoted form should be hidden
^:kindly/hide-code (+ 2 3) ;; this form should be hidden
^:kindly/hide-result (def my-var 3) ;; results of this form should be hidden

;; This is one paragraph of a comment.

;; And this is a second paragraph of a comment, because of the 2+ linebreaks.

;; This is the third paragraph. No clojure forms between; 3 paragraph
 ;; elements.
;; This text is in the third paragraph despite having a newline separating it.

^{:kindly/kind :kind/hiccup} [:h2 "Error example"]
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
