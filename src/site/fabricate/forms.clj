(ns site.fabricate.forms
  (:require [malli.core :as m]))

(def kindly-form-schema
  "Malli schema defining the map elements expected by Kindly's intermediate representation"
  (m/schema
   [:map
    {:description
     "Malli schema defining the map elements expected by Kindly's intermediate representation"}
    [:code
     {:description "The source code of a form that produced a Kindly value"}
     :string]
    [:form {:description "The Clojure form that produced a Kindly value"} :any]
    [:value {:description "The Kindly value returned by a Clojure form"} :any]
    [:kind {:description "The Kindly kind annotation for the value"} :keyword]
    [:kindly/hide-code
     {:description "Whether to hide the source expression in the output"
      :optional    true} :boolean]
    [:kindly/hide-result
     {:description "Whether to hide the result of evaluation in the output"
      :optional    true} :boolean]]))
