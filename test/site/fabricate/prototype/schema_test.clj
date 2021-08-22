(ns site.fabricate.prototype.schema-test
  (:require [site.fabricate.prototype.schema :refer :all]
            [malli.error :as me]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.test :as t]))


(t/deftest transformations
  (t/testing "unname schema"
    (t/is
     (mu/equals
      [:cat [:int {:name "a"}] :string
       [:schema [:+ :int]]]
      (unname-schema
       [:catn [:a [:int {:name "a"}]] [:b :string]
        [:c [:schema [:+ :int]]]])))

    (t/is
     (mu/equals
      [:or [:int {:name "a"}] :string
       [:schema [:+ [:alt :int :string]]]]

      (unname-schema
       [:orn [:a [:int {:name "a"}]] [:b :string]
        [:c [:schema [:+ [:altn [:i :int] [:s :string]]]]]])))

    (t/is (mu/equals
           [:or
            [:= "b"]
            [:schema [:+ [:alt :int :string]]]]
           (unname-schema
            [:or
             [:= "b"]
             [:schema [:+ [:altn [:i :int] [:s :string]]]]])))

    (t/is
     (mu/equals
      [:schema
       {:registry
        {:i :int
         :s :string
         :sequence [:cat :i :s
                    [:schema [:+ [:alt :i :s]]]]}}
       :sequence]
      (unname-schema
       [:schema
        {:registry
         {:i :int
          :s :string
          :sequence
          [:catn [:i :i] [:s :s]
           [:subsequence [:schema [:+ [:altn [:i :i] [:s :s]]]]]]}}
        :sequence])))

    (t/is
     (mu/equals
      [:schema
       {:registry
        {::i :int
         ::s :string
         ::sequence
         [:or [:schema [:ref ::i]] [:schema [:ref ::s]]
          [:schema [:+ [:alt [:schema [:ref ::i]]
                        [:schema [:ref ::s]]]]]]}}
       ::sequence]
      (unname-schema
       [:schema
    {:registry
     {::i :int
      ::s :string
      ::sequence
      [:orn [:i [:schema [:ref ::i]]] [:s [:schema [:ref ::s]]]
       [:subsequence [:schema [:+ [:altn [:i [:schema [:ref ::i]]]
                                   [:s [:schema [:ref ::s]]]]]]]]}}
        ::sequence])))
    ))

(comment

  (mu/to-map-syntax
   [:schema
    {:registry
     {:i :int
      :s :string
      :sequence [:catn [:i :i] [:s :s]
                 [:schema [:+ [:altn [:i :i] [:s :s]]]]]}}
    :sequence])

  (m/validate
   [:schema
    {:registry
     {::i :int
      ::s :string
      ::sequence
      [:orn [:i [:schema [:ref ::i]]] [:s [:schema [:ref ::s]]]
       [:subsequence [:schema [:+ [:altn [:i [:schema [:ref ::i]]]
                                   [:s [:schema [:ref ::s]]]]]]]]}}
    ::sequence]
   [1])


  (mu/subschemas
   [:schema
    {:registry
     {:i :int
      :s :string
      :sequence
      [:orn [:i [:schema [:ref :i]]] [:s [:schema [:ref :s]]]
       [:subsequence [:schema [:+ [:altn [:i [:schema [:ref :i]]]
                                   [:s [:schema [:ref :s]]]]]]]]}}
    :sequence])

  )
