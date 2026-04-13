(ns site.fabricate.dev.html
  "Dev-time checks for HTML. Requires the optional `nu.validator` library (see the :dev alias in deps.edn)"
  (:require [clojure.data.json :as json])
  (:import [nu.validator.validation SimpleDocumentValidator]
           [nu.validator.client EmbeddedValidator]
           [java.io ByteArrayInputStream]))


(def html-validator
  (doto (EmbeddedValidator.)
    (.setOutputFormat nu.validator.client.EmbeddedValidator$OutputFormat/JSON)))

(defn validate-html-string
  "Returns any warnings or errors from the HTML string as a sequence of parsed JSON maps."
  [html-string]
  (let [html-input-stream (ByteArrayInputStream. (.getBytes html-string))]
    (try (let [validator-output (.validate html-validator html-input-stream)]
           (if (empty? validator-output) {} (json/read-str validator-output)))
         (catch Exception e (Throwable->map e)))))
