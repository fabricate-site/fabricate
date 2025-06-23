(ns site.fabricate.prototype.kindly-test
  (:require [site.fabricate.prototype.kindly :as kindly]
            [malli.core :as m]
            [clojure.test :as t]))



(t/deftest data-model
  (t/testing "kindly"
    (t/is (m/validate kindly/Form
                      {:code  "(into [:div] (for [i (range 9)] [:p i]))"
                       :form  '(kind/hiccup
                                (into [:div] (for [i (range 9)] [:p i])))
                       :value ^{:kindly/kind :kind/hiccup}
                              (into [:div] (for [i (range 9)] [:p i]))
                       :kind  :kind/hiccup}))))
