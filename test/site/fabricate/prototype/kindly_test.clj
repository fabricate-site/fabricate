(ns site.fabricate.prototype.kindly-test
  (:require [site.fabricate.prototype.kindly :as k]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [scicloj.kindly-advice.v1.api :as kindly-advice]
            [scicloj.kindly-advice.v1.advisors :as advisors]
            [malli.core :as m]
            [clojure.test :as t]))



(t/deftest data-model
  (t/testing "kindly"
    (t/is (m/validate k/Form
                      {:code  "(into [:div] (for [i (range 9)] [:p i]))"
                       :form  '(kind/hiccup
                                (into [:div] (for [i (range 9)] [:p i])))
                       :value ^{:kindly/kind :kind/hiccup}
                              (into [:div] (for [i (range 9)] [:p i]))
                       :kind  :kind/hiccup}))))

(t/deftest functions (t/is (k/wrapped-value? (kind/code 3))))


(t/deftest kindly-normalization
  (t/testing "kindly-advice"
    (t/testing "general capabilities"
      (let [nested-kindly-form
            ^{:kindly/kind :kind/hiccup}
            [:div {:class "basic-hiccup"} [:p "General paragraph text"]
             [:figure
              [:div
               ^{:kindly/kind :kind/vega-lite}
               {:encoding {:y    {:field "y" :type "quantitative"}
                           :size {:value 400}
                           :x    {:field "x" :type "quantitative"}}
                :mark     {:type "circle" :tooltip true}
                :data     {:values "x,y\n1,1\n2,-4\n3,9\n"
                           :format {:type "csv"}}}]
              [:figcaption
               "Custom data visualization using nested Kindly forms"]]]
            advised-form (kindly-advice/advise {:value nested-kindly-form})]
        (t/is (map? advised-form) "Outer normalization should work")
        (t/is (let [vl-spec (get-in advised-form [:value 3 1 1])]
                (and (map? vl-spec) (contains? vl-spec :value)))
              "kindly-advice should recursively normalize forms")))
    (t/testing "fabricate compatibility")))


(comment
  (kindly/ad)
  (kindly-advice/advise
   {:value
    ^{:kindly/kind :kind/hiccup}
    [:div {:class "basic-hiccup"} [:p "General paragraph text"]
     [:figure
      [:div
       ^{:kindly/kind :kind/vega-lite}
       {:encoding {:y    {:field "y" :type "quantitative"}
                   :size {:value 400}
                   :x    {:field "x" :type "quantitative"}}
        :mark     {:type "circle" :tooltip true}
        :data     {:values "x,y\n1,1\n2,-4\n3,9\n" :format {:type "csv"}}}]
      [:figcaption "Custom data visualization using nested Kindly forms"]]]}))
