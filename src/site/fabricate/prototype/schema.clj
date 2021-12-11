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
       (catch Exception e
         #_(prn (Throwable->map e))
         false)))

(defn has-reqd?
  "Checks to see if at least one entry in the given map schema has required keys"
  {:malli/schema (m/schema [:=> [:cat [:fn malli?]] :boolean])}
  [schema]
  (let [[s-type & entries] (m/form schema)]
    (and
     (= :map s-type)
     (some (fn [e] (not (and (map? (second e))
                             (get (second e) :optional))))
           entries))))

(defn subschema
  {:malli/schema (m/schema [:=> [:cat [:fn malli?] :any] [:fn malli?]])}
  [[_ meta-map orig-ref] new-ref]
  [:schema meta-map new-ref])

(def regex [:fn #(instance? java.util.regex.Pattern %)])

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

(defn or->orn
  "Transforms a unnamed :or schema into an indexed :orn (courtesy of Tommi Reiman)"
  {:malli/schema (m/schema [:=> [:cat [:fn malli?]] [:fn malli?]])}
  [s]
  (m/into-schema :orn (m/properties s) (map-indexed vector (m/children s))))

;; (comment
;;   (assert (or->orn [:or :int :boolean])

;;           [:orn [0 :int] [1 :boolean]])

;;   )



(defn unify
  "A lighter-weight version of malli's own unify/merge that's
  more compatible with number-based indexing/item access"
  {:malli/schema (m/schema [:=> [:cat [:* [:fn malli?]]]
                            [:fn malli?]])}
  [schemas]
  (into [:orn] (map-indexed vector schemas)))
