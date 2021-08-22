(ns site.fabricate.prototype.schema-test
  (:require [site.fabricate.prototype.schema :refer :all]
            [malli.error :as me]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.test :as t]))


(t/deftest transformations
  (t/testing "unname schema"
    (t/is
     (= (mu/to-map-syntax
         [:cat [:int {:name "a"}] :string
          [:schema [:+ :int]]])
        (mu/to-map-syntax (unname-schema
                           [:catn [:a [:int {:name "a"}]] [:b :string]
                            [:c [:schema [:+ :int]]]]))))

    (t/is
     (= (mu/to-map-syntax
         [:or [:int {:name "a"}] :string
          [:schema [:+ [:alt :int :string]]]])
        (mu/to-map-syntax
         (unname-schema
          [:orn [:a [:int {:name "a"}]] [:b :string]
           [:c [:schema [:+ [:altn [:i :int] [:s :string]]]]]]))))
    (t/is
     (= (mu/to-map-syntax
         [:schema
          {:registry
           {:i :int
            :s :string
            :sequence [:or :i :s
                       [:schema [:+ [:alt :i :s]]]]}}
          :sequence])
        (mu/to-map-syntax
         (unname-schema
          [:schema
          {:registry
           {:i :int
            :s :string
            :sequence [:orn [:i :i] [:s :s]
                       [:schema [:+ [:altn [:i :i] [:s :s]]]]]}}
           :sequence]))))
    ))
