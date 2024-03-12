(ns site.fabricate.methods.clojure
  "Fabricate namespace defining methods for turning Clojure namespaces into Hiccup documents"
  (:require [hiccup2.core :as hiccup]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as zip]
            [malli.core :as m]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [site.fabricate.prototype.page :as page]))




;; in the assemble step, should the Clojure code be treated as a "block"?

;; it probably doesn't make sense to fully tokenize it into hiccup nodes

;; that are only required for display purposes

;; this means that this namespace has two purposes:
;; 1. generate "semantic" Hiccup from Clojure source code while evaluating it
;; 2. define a way for Clojure source to be tokenized into hiccup for
;; server-side syntax highlighting

(def form-map-schema
  (m/schema
   [:map [:clojure/source {:description "Source of the expression as a string"} :string]
    [:clojure/form {:description "Parsed Clojure expression (unevaluated)"} :any]
    [:clojure.form/metadata {:description "Metadata of form" :optional true} :map]
    [:input/file {:description "Source file (relative to project)"} [:fn fs/exists?]]
    [:clojure/results {:description "Results of evaluation" :optional true} :any]]))

(defn file->forms
  "Generate a sequence of Clojure form maps from the input file."
  {:malli/schema (m/schema [:=> [:cat [:fn fs/exists?]] [:* form-map-schema]])}
  [f]
  (let [parsed  (parser/parse-file-all f)]
    ;; the zip API may make more sense here
    (mapv (fn [n]
            (let [src-info (reduce-kv (fn [m k v]
                                        (assoc m (keyword "clojure.source" (name k)) v))
                                      {}
                                      (meta n))]
              (merge src-info
                     {:input/file f
                      :clojure/source (node/string n)
                      :clojure/node n
                      :clojure/form (when (node/sexpr-able? n) (node/sexpr n))})))
          (:children parsed)))
  )



(comment
  (keyword "ns" (name :kw))

  (node/sexpr (keys (node/whitespace-node "   ")))

  (file->forms (fs/file "src/site/fabricate/api.clj"))

  (let [frm ^{:k "v"} '(+ 3 4)]
    (meta frm)
    )

  (require '[site.fabricate.api :as api])
  (def api-parsed
    (mapv
     #(try (page/node->hiccup %) (catch Exception e {:node % :status :error}))
     (:children (p/parse-string-all (slurp "src/site/fabricate/api.clj")))))
  api-parsed
  (page/node->hiccup (get-in (filterv #(= :error (:status %)) api-parsed)
                             [0 :node])))
