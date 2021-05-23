(ns site.fabricate.schema
  (:require [malli.core :as m]))

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
