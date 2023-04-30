(ns site.fabricate.prototype.docs-test
  (:require
   [site.fabricate.prototype.docs :refer :all]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.read.grammar :as grammar]
   [site.fabricate.prototype.page :as page]
   [hiccup2.core :as hiccup]
   [clojure.walk :refer [postwalk]]
   [clojure.test :as t]))


(def example-section "✳//[:section \n some text ]//🔚")
(def example-template "text and then ✳=[:em \"some emphasized text\" ]🔚")

(t/deftest helper-fns
  (t/is (any?
         (hiccup/html
          {:escape-strings? false}
          (template->example-hiccup example-template "non-existent-file.html.fab") )  )
        "Examples containing extended form expressions should produce valid Hiccup elements")
  (t/is (any?
         (hiccup/html
          {:escape-strings? false}
          (template->example-hiccup example-section "non-existent-file.html.fab") )  )
        "Examples containing extended form expressions should produce valid Hiccup elements"))

(comment
  (meta (first (read/parse example-section) ))

  (grammar/template example-template)
  (read/extended-form->form (second (grammar/template example-section)))

  (read/parse example-template)

  )
