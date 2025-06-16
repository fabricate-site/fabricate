(ns site.fabricate.prototype.eval
  "Prototype namespace for evaluating Clojure code and forms.")

;; TODOs: namespace support (?)
(defn eval-form
  [{:keys [form value] :as form-map}]
  (if (contains? form-map :value)
    form-map
    (try (let [result (eval form)] (assoc form-map :value result))
         (catch Exception e (assoc form-map :error (Throwable->map e))))))
