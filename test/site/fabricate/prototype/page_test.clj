(ns site.fabricate.prototype.page-test
  (:require [site.fabricate.prototype.page :as page]
            [site.fabricate.prototype.html :as html]
            [site.fabricate.api :as api]
            [malli.core :as m]
            [malli.error :as me]
            [clojure.test :as t]))

(def valid-block? (::html/pre html/element-validators))
(def valid-element? (m/validator html/element))

(t/deftest kinds
  (doseq [value [:a '(+ 1 2 3) 45 {:b 3}]]
    (t/testing (str "value: " value)
      (t/testing "code"
        (t/is (valid-block? (api/display-form {:kind :code :value value}
                                              {:site.fabricate.page/format
                                               :hiccup/html}))))
      (t/testing "edn"
        (t/is (valid-block? (api/display-form {:kind :edn :value value}
                                              {:site.fabricate.page/format
                                               :hiccup/html}))))))
  (t/testing "hiccup"
    (doseq [hiccup [[:span "text"] [:code "(map dec (range 29 58 2))"]]]
      (t/is (valid-element? (api/display-form {:kind  :hiccup
                                               :value hiccup}))))))
