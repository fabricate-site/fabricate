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

(def nested-hiccup
  {:value ^{:kind :kind/hiccup} [:div "text" ^:kind/hiccup [:p "more text"]]})

(def nested-kindly-value
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
     [:figcaption "Custom data visualization using nested Kindly forms"]]]})

(t/deftest functions
  (t/is (k/wrapped-value? (kind/code 3)))
  (t/is (k/context-map? nested-kindly-value)))

(t/deftest kindly-normalization
  (t/testing "kindly normalization"
    (t/is (= nested-hiccup
             (select-keys (k/normalize-value nested-hiccup) [:value]))
          "Normalization of context maps should leave their values unchanged")
    (t/is (= nested-hiccup
             (select-keys (k/normalize-value (kindly-advice/advise
                                              nested-hiccup))
                          [:value]))
          "Normalization of context maps should leave their values unchanged")
    (t/is (= nested-kindly-value
             (select-keys (k/normalize-value (kindly-advice/advise
                                              nested-kindly-value))
                          [:value]))
          "Normalization of context maps should leave their values unchanged")
    (t/is
     (= nested-kindly-value
        (select-keys (k/normalize-value nested-kindly-value) [:value]))
     "Normalization of advised context maps should leave their values unchanged")
    (let [normalized-hiccup (k/normalize-all nested-hiccup)
          normalized-form   (k/normalize-all nested-kindly-value)]
      (t/is (k/context-map? normalized-form) "Outer normalization should work")
      (t/is (k/context-map? normalized-hiccup)
            "Outer normalization should work")
      (t/is (let [vl-spec (get-in normalized-form [:value 3 1 1])]
              (and (map? vl-spec) (contains? vl-spec :value)))
            "kindly namespace should recursively normalize forms"))
    (t/testing "fabricate compatibility")))
