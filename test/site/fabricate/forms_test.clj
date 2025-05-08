(ns site.fabricate.forms-test
  (:require [site.fabricate.forms :as forms]
            [malli.core :as m]
            [clojure.test :as t]))

(t/deftest data-model
  (t/testing "kindly"
    (t/is (m/validate forms/kindly-form-schema
                      {:code  "(into [:div] (for [i (range 9)] [:p i]))"
                       :form  '(kind/hiccup
                                (into [:div] (for [i (range 9)] [:p i])))
                       :value ^{:kindly/kind :kind/hiccup}
                              (into [:div] (for [i (range 9)] [:p i]))
                       :kind  :kind/hiccup}))))
