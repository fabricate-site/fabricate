(ns site.fabricate.prototype.schema
  "Utility namespace for working with malli schemas."
  {:reference "https://github.com/metosin/malli"}
  (:require [malli.core :as m]
            [clojure.spec.alpha :as spec]))

(defn malli?
  "Returns true if the given form is a valid malli schema"
  {:malli/schema (m/schema [:=> [:cat :any] :boolean])}
  [form]
  (try (do (m/schema form) true)
       (catch Exception e false)))

(defn has-reqd?
  "Checks to see if at least one entry in the given map schema has required keys"
  {:malli/schema (m/schema [:=> [:cat [:fn malli?]] :boolean])}
  [schema]
  (let [s-type  (first (m/form schema))
        entries (m/children schema)]
    (and
     (= :map s-type)
     (some? (some (fn [[k props type]]
                    (or (nil? props)
                        (not (get props :optional))))
                  entries)))))

(defn subschema
  "Uses the registry of the provided schema to create a schema for one of the reference schemas in that registry."
  {:malli/schema (m/schema [:=> [:cat [:fn malli?] [:or :keyword :string]] [:fn malli?]])}
  [schema new-ref]
  (let [{:keys [registry]
         :as props} (m/properties schema)]
    (m/schema [:schema props new-ref])))

(def regex
  "Malli schema for regular expressions"
  (m/schema [:fn #(instance? java.util.regex.Pattern %)]))

(defn ns-form?
  "Returns true if the given form is a valid Clojure (ns ...) special form"
  {:malli/schema (m/schema [:=> [:cat :any] :boolean])}
  [form]
  (let [form (if (and (sequential? form) (= 'quote (first form))) (last form) form)]
    (not=
     :clojure.spec.alpha/invalid
     (spec/conform
      (spec/cat
       :ns-sym #(= % 'ns)
       :ns-name simple-symbol?
       :attr-map (spec/? map?)
       :ns-clauses :clojure.core.specs.alpha/ns-clauses)
      form))))

(defn unify
  "A lighter-weight version of malli's own unify/merge that's
  more compatible with number-based indexing/item access"
  {:malli/schema (m/schema [:=> [:cat [:* [:fn malli?]]]
                            [:fn malli?]])}
  [schemas]
  (m/into-schema :orn {} (map-indexed vector schemas)))

(def throwable-map-schema
  "Malli schema for the results of Throwable->map"
  (m/schema
   [:map
    [:cause :any]
    [:phase {:optional true} :any]
    [:via [:* [:map [:type :symbol]
               [:message :string]
               [:at [:vector :any]]
               [:data {:optional true} :any]]]]
    [:trace [:vector :any]]]))

(def
  ^{:malli/schema [:=> [:cat :any] :boolean]}
  throwable-map? (m/validator throwable-map-schema))
