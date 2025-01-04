(ns site.fabricate.prototype.source.clojure
  "Fabricate namespace defining methods for turning Clojure namespaces into Hiccup documents"
  (:require [hiccup2.core :as hiccup]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as zip]
            [malli.core :as m]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))


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
    [:clojure/result {:description "Results of evaluation" :optional true} :any]
    [:clojure/comment {:description "Clojure comment as string" :optional true}
     :string]
    [:text
     {:description "Clojure comment text, without leading semicolon"
      :optional    true} :string]]))

(defn- meta-node->metadata-map
  "Normalize the metadata node by converting bare keywords to map entries and return it as a Clojure value."
  [m-node]
  (let [node-meta (when (= :meta (:tag m-node)) (first (:children m-node)))
        kw?       (contains? node-meta :k)
        type-tag? (contains? node-meta :value)]
    (cond kw?       {(:k node-meta) true}
          type-tag? {:type (:value node-meta)}
          (nil? node-meta) (throw (ex-info "No metadata value" {:node m-node}))
          :default  (node/sexpr node-meta))))


;; should this be a multimethod?
(defn normalize-node
  "Return a map representing the 'value' for the given node"
  [n]
  (let [t      (node/tag n)
        src    (str n)
        result (case t
                 :comment    {:clojure/comment (str n) :text ""}
                 :newline    {:clojure/newlines (str n) :line-count 0}
                 :whitespace {:clojure/whitespace (str n)}
                 :meta       (let [meta-node (first (:children n))
                                   base-node (peek (:children n))]
                               (assoc (normalize-node base-node)
                                      :clojure/metadata
                                      (node/sexpr meta-node)))
                 (if (node/sexpr-able? n)
                   {:clojure/node n :clojure/form (node/sexpr n)}
                   {:type :unknown :node/tag t :clojure/node n}))]
    (assoc result :clojure/source src :clojure/node n :node/tag t)))

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
                            (meta n))]
    (merge src-info (normalize-node n))))

(defn file->forms
  "Generate a sequence of Clojure form maps from the input file."
  {:malli/schema (m/schema [:=> [:cat [:fn fs/exists?]] [:* form-map-schema]])}
  [clj-file]
  (let [parsed (parser/parse-file-all clj-file)]
    (mapv #(node->map % {:input/file clj-file}) (:children parsed))))

(defn eval-form
  [{clojure-form :clojure/form :as unevaluated-form}]
  (if clojure-form
    (let [start-time  (System/currentTimeMillis)
          eval-output (try {:clojure/result        (eval clojure-form)
                            :clojure.eval/duration (- (System/currentTimeMillis)
                                                      start-time)}
                           (catch Exception e
                             {:clojure/error (Throwable->map e)}))]
      (merge unevaluated-form eval-output))
    unevaluated-form))

;; should there be any "top level" stuff?

;; yes. there should be a map defining the namespace for the file and other
;; metadata.
(defn eval-forms
  "Evaluate the Clojure forms in a parsed file."
  {:malli/schema (m/schema [:-> [:* form-map-schema] [:* form-map-schema]])}
  [forms]
  (let [evaluated-forms (mapv eval-form forms)]
    {:clojure/namespace (ns-name *ns*)
     :clojure/forms     evaluated-forms
     :input/file        (:input/file (first evaluated-forms))}))

(comment
  (ns-name *ns*)
  (kind/md (str (parser/parse-string ";; a comment")))
  (parser/parse-string "^:kindly/hide-code '(quoted-form a b c)")
  (kindly/hide-code "abc")
  (kindly/consider)
  (kin)
  (file->forms *file*))


;; implementation working notes

;; metadata nodes - tricky as usual. reasonable strategy to normalize them
