(ns site.fabricate.prototype.eval
  "Prototype namespace for evaluating Clojure code and forms."
  (:require [site.fabricate.prototype.kindly]
            [malli.core :as m]
            [clojure.set :as set])
  (:import [clojure.lang IObj]))


;; I think it makes sense to move the schemas for parsed and evaluated
;; expressions to this namespace rather than keep them in the read namespace.

;; That way, this namespace can be the source of truth for what keys should and
;; shouldn't be expected in the course of evaluating Kindly form maps. It can
;; also be where the error messages are handled in more detail.

;; TODOs: namespace support (?)

#_(def Evaluated-Form
    (-> site.fabricate.prototype.kindly/Form
        (mu/merge [:map
                   [:error
                    {:optional true :description "Result of Throwable->map"}
                    :map] [:file]])))



(defn eval-form
  "Evaluate the form in the given Map."
  ;; TODO: refine the schema to reflect pre and post-eval keys
  {:malli/schema (m/schema [:-> site.fabricate.prototype.kindly/Form
                            site.fabricate.prototype.kindly/Form])}
  [{:keys [form value ns] :as form-map}]
  (if (contains? form-map :value)
    form-map
    (try (let [result (binding [*ns* (or ns *ns*)] (eval form))]
           (if (instance? IObj result)
             (let [result-meta     (meta result)
                   new-result-meta (-> form-map
                                       (set/rename-keys {:file/start-line :line
                                                         :file/start-column
                                                         :column})
                                       (select-keys [:ns :file :line :column]))]
               (alter-meta! result merge new-result-meta)))
           (merge form-map
                  {:value result}
                  (when (meta result) {:meta (meta result)})))
         (catch Exception e (assoc form-map :error (Throwable->map e))))))
