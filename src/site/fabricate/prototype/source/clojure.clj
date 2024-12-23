(ns site.fabricate.prototype.source.clojure
  "Fabricate namespace defining methods for turning Clojure namespaces into Hiccup documents"
  (:require [hiccup2.core :as hiccup]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as zip]
            [malli.core :as m]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]))




;; in the assemble step, should the Clojure code be treated as a "block"?

;; it probably doesn't make sense to fully tokenize it into hiccup nodes

;; that are only required for display purposes

;; this means that this namespace has two purposes:
;; 1. generate "semantic" Hiccup from Clojure source code while evaluating it
;; 2. define a way for Clojure source to be tokenized into hiccup for
;; server-side syntax highlighting

(def form-map-schema
  "Schema describing a simplified way of storing Clojure forms and their potential results."
  (m/schema
   [:map
    [:clojure/source {:description "Source of the expression as a string"}
     :string]
    [:clojure/form
     {:description "Parsed Clojure expression (unevaluated)" :optional true}
     :any]
    [:clojure.form/metadata {:description "Metadata of form" :optional true}
     :map]
    [:input/file {:description "Source file (relative to project)"}
     [:fn fs/exists?]]
    [:clojure/results {:description "Results of evaluation" :optional true}
     :any]
    [:clojure/comment {:description "Clojure comment as string" :optional true}
     :string]
    [:text
     {:description "Clojure comment text, without leading semicolon"
      :optional    true} :string]]))

(defn normalize-node
  "Return a map representing the 'value' for the given node"
  [n]
  (let [t (node/tag n)]
    (case t
      :comment    {:clojure/comment (str n) :text "" :clojure/node n}
      :newline    {:clojure/newlines (str n) :line-count 0 :clojure/node n}
      :whitespace {:clojure/whitespace (str n) :clojure/node n}
      (if (node/sexpr-able? n)
        {:clojure/node n :clojure/expr (node/sexpr n)}
        {:type :unknown :node/tag t :clojure/node n}))))


(def ^:private comment-pattern #"(?:;+\s*)([\s\S]*)")

(defn- extract-comment-text
  [comment-str]
  (last (re-matches comment-pattern comment-str)))


(defn node->map
  "Convert the given rewrite-clj node into a form map."
  [n m]
  (let [src-info (reduce-kv (fn [mm k v]
                              (assoc mm (keyword "clojure.source" (name k)) v))
                            m
                            (meta n))
        form     (when (node/sexpr-able? n) (node/sexpr n))
        comment? (node/comment? n)]
    (merge src-info
           {:clojure/source (node/string n) :clojure/node n}
           (when (meta form) {:clojure.form/metadata (meta form)})
           (when form {:clojure/form form})
           (when comment?
             {:clojure/comment (str n) :text (extract-comment-text (str n))}))))

(defn file->forms
  "Generate a sequence of Clojure form maps from the input file."
  {:malli/schema (m/schema [:=> [:cat [:fn fs/exists?]] [:* form-map-schema]])}
  [f]
  (let [parsed (parser/parse-file-all f)]
    (mapv #(node->map % {:input/file f}) (:children parsed))))

(comment
  (str (parser/parse-string ";; a comment"))
  (file->forms *file*))
