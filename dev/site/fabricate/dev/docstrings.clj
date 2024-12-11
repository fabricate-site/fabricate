(ns site.fabricate.dev.docstrings
  "Tools for working with docstrings"
  (:require [cybermonday.core :as md]
            [cybermonday.ir :as md-ir]
            [cybermonday.parser :as parser]
            [clojure.tools.reader :as reader]
            [site.fabricate.dev.source.markdown :as markdown]
            [site.fabricate.dev.references :as refs]))

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

#_(defn match-text
    [text]
    (cond (re-matches #"^'.*" text)
          {:text text :type :symbol :symbol (reader/read-string contents)}
          (re-matches #"^#'" text)
          {:text text :type :var :var (reader/read-string contents)}
          (re-matches #"^#:" text)
          {:text text :type :keyword :keyword (reader/read-string contents)}
          :default {:text text :type :term :term text}))

;; multimethod? dispatch on :type here?
#_(defn standardize
    [term]
    (let [term-data (match-text term)]
      (cond (:term term-data)    nil
            (:var term-data)     nil
            (:symbol term-data)  nil
            (:keyword term-data) nil)))

#_(defn md-lower-fn
    [[_kw {:keys [reference] :as m} text]]
    (if-not (nil? reference)
      [:a {:href reference} text]
      (let [{:keys [link hiccup-data] :as std} (standardize text)]
        [:a {:href link} hiccup-data])))

(comment
  (resolve 'my-fake-ns/var)
  (def example-docstring
    "Return the data with ...something, after calling [[my-ns/fn]] on it")
  (def example-docstring-2
    "Return the data with ...something, after calling [[my-ns/fn|`alias`]] on it")
  (markdown/md->hiccup example-docstring)
  (markdown/md->hiccup example-docstring-2)
  (parser/to-hiccup (parser/parse example-docstring) example-docstring)
  (println (md/parse-body
            "Return the data with ...something, after calling [[some-fn]] on it"
            {})))
