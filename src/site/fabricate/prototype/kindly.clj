(ns site.fabricate.prototype.kindly
  "Namespace for defining and working with kindly values as maps or annotated Clojure data."
  (:require [malli.core :as m]
            [malli.util :as mu]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [scicloj.kindly-advice.v1.api :as kindly-advice]
            [scicloj.kindly-advice.v1.advisors :as advisors]
            [clojure.walk :as walk]))

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

(defn metadata-annotation?
  [v]
  (let [value-meta (meta v)]
    (or (contains? value-meta :kind)
        (contains? value-meta :kindly/kind)
        ;; for values annotated like ^:kind/md or ^kind/md
        (some (fn [[mk mv]]
                (and (or (keyword? mk) (symbol? mk))
                     (= "kind" (namespace mk))
                     (true? mv)))
              value-meta))))

(defn wrapped-value?
  "Check whether the value is a 'wrapped' value that carries Kindly annotations"
  {:malli/schema (m/schema [:-> :any :boolean])}
  [v]
  (and (vector? v) (get-in (meta v) [:kindly/options :wrapped-value])))

(defn- context-map-value-metadata
  {:malli/schema (m/schema [:-> :map :map])}
  [v]
  (meta (or (:value v) (:kindly/value v) (:form v) (:kindly/form v))))

(defn context-map?
  "Check whether the value is a context map with Kindly metadata"
  {:malli/schema (m/schema [:-> :any :boolean])}
  [v]
  (and (map? v)
       (or (contains? v :value)
           (contains? v :kindly/value)
           (contains? v :form)
           (contains? v :kindly/form))
       (or (contains? v :kind)
           (contains? v :kindly/kind)
           ;; corner case: a value won't always propagate kindly
           ;; annotations to its wrapping context map
           (let [context-value-metadata (context-map-value-metadata v)]
             (or (contains? context-value-metadata :kind)
                 (contains? context-value-metadata :kindly/kind))))))

(defn advised?
  "Returns true if the value has already been advised by kindly-advice."
  [v]
  (and (context-map? v) (contains? v :advice)))

(defn kindly?
  "Returns true if the value is annotated with Kindly metadata."
  {:malli/schema (m/schema [:-> :any :boolean])}
  [v]
  (cond (metadata-annotation? v) true
        (wrapped-value? v) true
        (context-map? v)   true
        (advised? v)       true
        :default           false))

(defn normalize-value
  "Normalize the kindly value to its context map representation, advising it using kindly-advice."
  {:malli/schema (m/schema [:-> :any :map])}
  [v]
  (cond (map-entry? v) v
        (and (kindly? v) (advised? v)) v
        (and (kindly? v) (context-map? v)) (kindly-advice/advise v)
        (and (kindly? v) (not (contains? v :value))) (kindly-advice/advise
                                                      {:value v})
        :default v))

(defn normalize-all
  "Normalize the kindly value to its context map representation, recursively advising its child elements, if present."
  [v]
  (let [result (walk/postwalk normalize-value v)]
    ;; this feels like a hack but it works around the expected behavior
    ;; of postwalk to produce a sensible result
    (if (and (map? result) (contains? result :value) (advised? (:value result)))
      (:value result)
      result)))
