(ns site.fabricate.prototype.kindly
  "Namespace for defining and working with kindly values as maps or annotated Clojure data."
  (:require [malli.core :as m]
            [malli.util :as mu]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))

#_(defn kindly? [v] () (and (instance? clojure.lang.IObj v)))

;; TODO: make this recursive (fragments can contain fragments)

(def Form
  "The map representation of a Clojure form & value used by Kindly"
  (m/schema
   [:map
    {:description
     "The map representation of a Clojure form & value used by Kindly"}
    [:code
     {:description "The source code of a form that produced a Kindly value"}
     :string]
    [:form {:description "The Clojure form that produced a Kindly value"} :any]
    [:value {:description "The Kindly value returned by a Clojure form"} :any]
    [:kind {:description "The Kindly kind annotation for the value"} :keyword]
    [:kindly/hide-code
     {:description "Whether to hide the source expression in the output"
      :optional    true} :boolean]
    [:kindly/options
     {:description "Additional options for kindly forms" :optional true}
     [:maybe
      [:map
       ;; :hide-value is an undocumented option from the kindly-render
       ;; library but it make sense to include it here because the
       ;; capability of hiding results is also required by the template
       ;; implementation
       [:hide-value
        {:optional true :description "Whether to hide the value in the output."}
        :boolean]]]]]
   #_[:schema {:registry {::form :map}}]))

(def Fragment
  "A Kindly fragment, a special Kindly value consisting of a vector of multiple kindly values."
  (mu/merge Form
            (m/schema
             [:map
              {:description
               "A Kindly fragment consisting of multiple kindly values"}
              [:kind {:description "The kindly type (fragment)"} [:= :fragment]]
              [:value {:description "The vector of kindly values"}
               [:sequential Form]]])))



(comment
  (kind/vector [3])
  (kind/vector 3)
  (= (kind/code 3) (kind/code [3]))
  (kind/fragment [(kind/code 'a) (kind/hiccup [:div "example"])]))


(defn wrapped-value?
  "Check whether the value is a 'wrapped' value that carries Kindly annotations"
  {:malli/schema (m/schema [:-> :any :boolean])}
  [v]
  (and (vector? v) (get-in (meta v) [:kindly/options :wrapped-value])))
