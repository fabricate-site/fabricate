(ns site.fabricate.prototype.eval
  "Prototype namespace for evaluating Clojure code and forms."
  (:require [site.fabricate.prototype.kindly]
            [site.fabricate.prototype.schema :as schema]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.set :as set])
  (:import [clojure.lang IObj Namespace IReference]))


;; I think it makes sense to move the schemas for parsed and evaluated
;; expressions to this namespace rather than keep them in the read namespace.

;; That way, this namespace can be the source of truth for what keys should and
;; shouldn't be expected in the course of evaluating Kindly form maps. It can
;; also be where the error messages are handled in more detail.


;; TODO: should all the schemas live in... the schema namespace?

(def Parsed-Form
  (-> site.fabricate.prototype.kindly/Form
      (mu/dissoc :value)
      ;; TODO: set default kind
      (mu/optional-keys [:kind])
      (mu/merge
       [:map
        [:parse-error
         {:optional true
          :description
          "Error message returned when Fabricate enounters a parsing error."}
         :map]
        [:file {:optional true :description "The source file for the form"}
         ;; this schema should be stricter - parsing should have
         ;; a predictable contract
         [:or :string [:fn schema/file?] :nil]]
        [:ns {:optional true :description "The namespace of the form"}
         [:fn #(instance? Namespace %)]]])
      (mu/update-properties assoc
                            :description
                            "A form parsed by Fabricate before evaluation")))


(def Evaluated-Form
  (-> site.fabricate.prototype.kindly/Form
      (mu/merge
       [:map
        [:error {:optional true :description "Result of Throwable->map"} :map]])
      (mu/update-properties assoc
                            :description
                            "A Kindly form that has been evaluated")))



(defn eval-form
  "Evaluate the form in the given Map."
  {:malli/schema (m/schema [:-> Parsed-Form Evaluated-Form])}
  [{:keys [form value ns] :as form-map}]
  (if (contains? form-map :value)
    form-map
    (try (let [result (binding [*ns* (or ns *ns*)] (eval form))]
           (cond (instance? IReference result)
                 (let [result-meta     (meta result)
                       new-result-meta (-> form-map
                                           (set/rename-keys
                                            {:file/start-line   :line
                                             :file/start-column :column})
                                           (select-keys [:ns :file :line
                                                         :column]))]
                   (alter-meta! result merge new-result-meta)))
           (merge form-map
                  {:value result}
                  (when (meta result) {:meta (meta result)})))
         (catch Exception e (assoc form-map :error (Throwable->map e))))))
