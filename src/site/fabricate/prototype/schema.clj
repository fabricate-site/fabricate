(ns site.fabricate.prototype.schema
  (:require [malli.core :as m]
            [malli.util :as mu]
            [clojure.spec.alpha :as spec]))

(defn has-reqd?
  "Checks to see if at least one entry in the given map schema has required keys"
  [schema]
  (let [[s-type & entries] (m/form schema)]
       (and
        (= :map s-type)
        (some (fn [e] (not (and (map? (second e))
                                (get (second e) :optional))))
              entries))))

(defn subschema [[_ meta-map orig-ref] new-ref]
  [:schema meta-map new-ref])

(defn ns-form?
  "Returns true if the given form is a valid Clojure (ns ...) special form"
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

(defn
  or->orn
  "Transforms a unnamed :or schema into an indexed :orn (courtesy of Tommi Reiman)"
  [s]
  (m/into-schema :orn (m/properties s) (map-indexed vector (m/children s))))

(comment
  (assert (or->orn [:or :int :boolean])

          [:orn [0 :int] [1 :boolean]])

  )

(defn malli?
  "Returns true if the given form is a valid malli schema"
  [form]
  (try (do (m/schema form) true)
       (catch Exception e
         #_(prn (Throwable->map e))
         false)))

(defn unify
  "A lighter-weight version of malli's own unify/merge that's
  more compatible with number-based indexing/item access"
  {:malli/schema [:=> [:cat [:+ [:fn malli?]]]
                  [:fn malli?]]}
  [schemas]
  (into [:orn] (map-indexed vector schemas)))

(defn -unname-children [schema]
  (let [updater (fn [children]
                  (map (fn [[name _ cs]] cs)
                       children))
        renames {:catn :cat
                 :orn :or
                 :altn :alt}
        ms (mu/to-map-syntax schema)
        res
        (cond
           (contains? renames (:type ms))
          (-> ms
              (update :type renames)
              (update :children updater))
          :else ms)]
    (mu/from-map-syntax res)))

(defn unname-schema
  "Transforms the schema and all subschemas to use unnamed alternation and concatenation"
  [schema]
  (m/walk
   schema
   (m/schema-walker -unname-children)))
