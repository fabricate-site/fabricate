(ns site.fabricate.dev.docstrings
  "Tools for working with docstrings"
  (:require [cybermonday.core :as md]
            [cybermonday.ir :as md-ir]
            [clojure.tools.reader :as reader]))

(defn extract-bracket-text
  [text]
  (let [[match contents] (re-matches #"(?:\[\[)(.*)(?:\]\])" text)]
    {:text match :contents contents}))

(defn parse-bracket
  [bracket-text]
  (cond (re-matches #"^\[\['.*" bracket-text)
        (let [{:keys [contents] :as ex} (extract-bracket-text bracket-text)]
          (assoc ex :type :symbol :symbol (reader/read-string contents)))
        (re-matches #"^\[\[#'.*" bracket-text)
        (let [{:keys [contents] :as ex} (extract-bracket-text bracket-text)]
          (assoc ex :type :var :var (reader/read-string contents)))
        (re-matches #"^\[\[:.*" bracket-text)
        (let [{:keys [contents] :as ex} (extract-bracket-text bracket-text)]
          (assoc ex :type :keyword :keyword (reader/read-string contents)))
        :default (merge (extract-bracket-text bracket-text)
                        {:type :site.fabricate/term})))

(comment
  (def example-docstring
    "Return the data with ...something, after calling [[my-ns/fn]] on it")
  (md/parse-body example-docstring {:lower-fns {:markdown/link-ref identity}})
  (md-ir/md-to-ir example-docstring)
  (println (md/parse-body
            "Return the data with ...something, after calling [[some-fn]] on it"
            {})))
