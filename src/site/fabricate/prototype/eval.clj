(ns site.fabricate.prototype.eval
  "Prototype namespace for evaluating Clojure code and forms."
  (:require [site.fabricate.prototype.kindly]
            [malli.core :as m]))

;; TODOs: namespace support (?)
(defn eval-form
  "Evaluate the form in the given Map."
  ;; TODO: refine the schema to reflect pre and post-eval keys
  {:malli/schema (m/schema [:-> site.fabricate.prototype.kindly/Form
                            site.fabricate.prototype.kindly/Form])}
  [{:keys [form value] :as form-map}]
  (if (contains? form-map :value)
    form-map
    (try (let [result (eval form)] (assoc form-map :value result))
         (catch Exception e (assoc form-map :error (Throwable->map e))))))
