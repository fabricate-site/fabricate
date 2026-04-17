(ns site.fabricate.dev.html
  "Dev-time checks for HTML. Requires the optional `nu.validator` library (see the :dev alias in deps.edn)"
  (:require [clojure.data.json :as json]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.string :as str])
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

(def ignored-errors
  "A set of error messages to ignore due to outdated validation rules in nu.validator"
  {:html/dl-child-div
   (m/schema
    [:re
     #"Element “dt” not allowed as child of element “div” in this context."])
   :html/heading-levels (m/schema [:re #"The heading .* skipping \d heading"])
   ;; fix this upstream in the manual eventually
   :html/code-ol
   (m/schema
    [:re
     #"Element “ol” not allowed as child of element “code” in this context."])
   :css/layer (m/schema [:re #"CSS.*layer"])
   :css/subgrid (m/schema [:re #"CSS.*subgrid"])
   :css/any (m/schema [:re #"CSS.*"])})

(defn- debug-error-msg [v _] (let [error-str (:value v)] error-str))

(def IgnorableErrorMessage
  (m/schema
   [:map ["type" [:= "error"]]
    ["message"
     [:re
      {:error/fn      debug-error-msg
       :error/message "Unhandled error in nu.validator output"}
      ;; a string that only matches one of the 'ignored errors'
      (re-pattern (str/join "|" (map #(last (m/form %)) (vals ignored-errors))))
      #_(into [:or]
              (map #(mu/update-properties % assoc :error/fn debug-error-msg))
              (vals ignored-errors))]]]))

(def ValidHTMLOutput
  (m/schema
   [:map {:description "parsed JSON output of nu.validator HTML/CSS validation"}
    ["messages"
     [:*
      [:or
       ;; info - warning is also safe to ignore
       [:map ["message" :string] ["type" [:= "info"]]
        ["subType" {:optional true} [:= "warning"]]]
       [:ref #'IgnorableErrorMessage]]]]]))

(comment
  (m/validate [:not [:re #"CSS"]] "HTML lacks required <head> elems")
  (m/explain (into [:and {:error/fn debug-error-msg} :string]
                   (map #(conj [:not] %))
                   (vals ignored-warnings))
             "CSS unrecognized @layer")
  (m/validate (into [:and {:error/fn debug-error-msg} :string]
                    (map #(conj [:not] %))
                    (vals ignored-warnings))
              "HTML lacks required head elem")
  (m/validate (into [:and {:error/fn debug-error-msg} :string]
                    (map #(conj [:not] %))
                    (vals ignored-warnings))
              "CSS @layer")
  (m/validate (into [:and {:error/fn debug-error-msg} :string]
                    (map #(conj [:not] %))
                    (vals ignored-warnings))
              "HTML lacks required <head> elems"))

(def ValidHTMLStrictOutput
  (m/schema
   [:map
    {:description
     "schema for parsed valid JSON output of nu.validator HTML/CSS validation. Strict validation: will not match if any errors are returned in the output."}
    ["messages" [:* [:map ["type" [:enum "info"]] ["message" :string]]]]]))
