(ns site.fabricate.prototype.kindly-test
  (:require [site.fabricate.prototype.kindly :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [malli.core :as m]
            [clojure.test :as t]))

(def example-nested-value
  ^{:kind :fragment}
  [^{:kind :hiccup}
   [:div {:class "example"}
    "An example Hiccup div containing a nested kindly element"
    (kind/md "Nested **markdown** text in a Hiccup element")]
   ^{:kind :code} '(+ 3 4 5 6)
   {:kind :md :value "_More_ markdown text inside of a Kindly map."}
   ^{:kind :edn} {:a 1 :b 2 :description "EDN map"}])

(t/deftest data-model
  (t/testing "kindly"
    (t/is (m/validate kindly/Kindly-Value example-nested-value))))
