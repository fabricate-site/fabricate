(ns site.fabricate.prototype.document.clojure
  "Fabricate namespace defining methods for turning Clojure namespaces into Hiccup documents"
  (:require [hiccup2.core :as hiccup]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as zip]
            [malli.core :as m]
            [babashka.fs :as fs]
            [clojure.java.io :as io]))




;; in the assemble step, should the Clojure code be treated as a "block"?

;; it probably doesn't make sense to fully tokenize it into hiccup nodes

;; that are only required for display purposes

;; this means that this namespace has two purposes:
;; 1. generate "semantic" Hiccup from Clojure source code while evaluating it
;; 2. define a way for Clojure source to be tokenized into hiccup for
;; server-side syntax highlighting

(def form-map-schema
  (m/schema [:map
             [:clojure/source
              {:description "Source of the expression as a string"} :string]
             [:clojure/form
              {:description "Parsed Clojure expression (unevaluated)"} :any]
             [:clojure.form/metadata
              {:description "Metadata of form" :optional true} :map]
             [:input/file {:description "Source file (relative to project)"}
              [:fn fs/exists?]]
             [:clojure/results
              {:description "Results of evaluation" :optional true} :any]]))

(defn node->map
  [n m]
  (let [src-info (reduce-kv (fn [mm k v]
                              (assoc mm (keyword "clojure.source" (name k)) v))
                            m
                            (meta n))
        form     (when (node/sexpr-able? n) (node/sexpr n))]
    (merge src-info
           {:clojure/source (node/string n) :clojure/node n :clojure/form form}
           (when (meta form) {:clojure.form/metadata (meta form)}))))

(defn file->forms
  "Generate a sequence of Clojure form maps from the input file."
  {:malli/schema (m/schema [:=> [:cat [:fn fs/exists?]] [:* form-map-schema]])}
  [f]
  (let [parsed (parser/parse-file-all f)]
    (mapv #(node->map % {:input/file f}) (:children parsed))))
