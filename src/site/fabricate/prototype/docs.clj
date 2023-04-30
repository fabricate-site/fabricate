(ns site.fabricate.prototype.docs
  (:require
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.read.grammar :as grammar]
   [site.fabricate.prototype.page :as page]
   [clojure.walk :refer [postwalk]]))

(def ^:private start (first grammar/delimiters))
(def ^:private end (second grammar/delimiters))

(defn fabricate-expr->example-hiccup [expr]
  (let [ctrl-char
        (cond
          (and (contains? expr :expr) (:display expr)) "+="
          (:display expr) "+"
          (contains? expr :expr) "="
          (contains? expr :exec) "")]
    [:span start ctrl-char
     (page/expr->hiccup (or (:expr expr) (:exec expr)))
     end]))

;; the parsed templates don't preserve enough metadata about
;; extended form expressions to effectively pattern match on them

(defn template->example-hiccup
  "Convert the given template to a Hiccup document without evaluating it."
  [template-text filename]
  (let [parsed (read/parse template-text filename)]
    (->> (read/parse template-text filename)
         (postwalk (fn replace? [i]
                     (cond
                       (read/fabricate-expr? i)
                       (fabricate-expr->example-hiccup i)
                       :else i)))
         (apply list))))

(comment

  )
