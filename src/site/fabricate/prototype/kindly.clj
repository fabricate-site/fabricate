(ns site.fabricate.prototype.kindly
  "Namespace for defining and working with kindly values as maps or annotated Clojure data."
  (:require [malli.core :as m]
            [malli.util :as mu]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))

;; See https://github.clerk.garden/respatialized/kindly-spec/ for background
;; information.

(def Kind-Properties
  (m/schema
   [:map
    {:description
     "The properties required by Kindly, either as a map or in the form of metadata."}
    [:kind {:description "The kind of the value"} :keyword]
    [:kindly/hide-code
     {:description "Whether to hide the source expression in the output"
      :optional    true} :boolean]
    [:kindly/options
     {:description
      "Additional options for the kind. May be kind-specific or general."
      :optional true}
     [:maybe
      [:map
       [:hide-value
        {:optional true :description "Whether to hide the value in the output."}
        :boolean]
       [:wrapped-value
        {:optional true
         :description
         "Whether the value has been 'wrapped' in a vector to carry Kindly metadata."
         :annotation "Not yet supported by Kindly"} :boolean]]]]]))


(def kindly-properties? (m/validator Kind-Properties))

(defn kindly-metadata?
  [v]
  (and (instance? clojure.lang.IObj v) (kindly-properties? (meta v))))

(defn non-meta-value? [v] (not (instance? clojure.lang.IObj v)))

(def Wrapped-Value (m/schema [:vector {:min 1 :max 1} [:fn non-meta-value?]]))

(defn wrapped-value?
  [v]
  (and (m/validate Wrapped-Value v) (m/validate Kind-Properties (meta v))))

(def Kindly-Value
  (m/schema
   [:schema
    {:registry
     ;; start from the top and proceed downward
     {:kindly/value [:or {:description "A Kindly value"}
                     [:ref :kindly/meta-value] [:ref :kindly/wrapped-val]
                     [:ref :kindly/map] [:ref :kindly/fragment]]
      :kindly/meta-value
      [:and {:description "A value with Kindly-specific metadata"}
       [:fn kindly-metadata?] [:ref :clojure/value] [:not [:ref :kindly/map]]]
      :kindly/wrapped-val
      [:and
       {:description "A plain value wrapped in a vector with Kindly metadata"}
       [:fn wrapped-value?] [:vector {:min 1 :max 1} :any]]
      :kindly/map
      (mu/merge Kind-Properties
                ;; the ref needs to be "pulled in" to
                ;; the subschema here, apparently
                [:map
                 {:registry    {:clojure/value [:ref :clojure/value]}
                  :description "A Kindly value as a plain Clojure map"}
                 [:code :string] [:form :any] [:value [:ref :clojure/value]]])
      :kindly/fragment
      [:or
       {:description "A Kindly fragment contains a sequence of Kindly values"}
       [:and [:fn kindly-metadata?] [:vector [:ref :kindly/value]]]
       (mu/merge Kind-Properties
                 [:map {:registry {:kindly/value [:ref :kindly/value]}}
                  [:code :string] [:form :any] [:kind [:= :fragment]]
                  [:value [:vector [:ref :kindly/value]]]])]
      :clojure/value
      [:or
       {:description
        "Kindly values are themselves Clojure values,
         but not all Clojure values are Kindly values."}
       [:and :any #_[:not [:ref :kindly/value]]]
       [:map-of [:ref :clojure/value] [:ref :clojure/value]]
       [:sequential [:ref :clojure/value]] [:set [:ref :clojure/value]]
       ;; putting the refs later ensures the base case gets found and
       ;; the stack doesn't blow up
       [:ref :kindly/value]]}} :kindly/value]))
