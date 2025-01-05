(ns site.fabricate.prototype.source.clojure
  "Fabricate namespace defining methods for turning Clojure namespaces into Hiccup documents"
  (:require [hiccup2.core :as hiccup]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as zip]
            [malli.core :as m]
            [babashka.fs :as fs]
            [clojure.tools.reader :as reader]
            [edamame.core :as e]
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
    [:clojure/namespace
     {:description
      "Primary/initial namespace of the file the form originates form"
      :optional true} :symbol]
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
                 :uneval     {:clojure/uneval (str n)}
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

(defn get-ns
  "Get the namespace symbol from a given node. Returns the first namespace."
  [node]
  (let [ns-loc (zip/find-value (zip/of-node node) zip/next 'ns)]
    (zip/sexpr (zip/next ns-loc))))

(comment
  (m/validate :symbol
              (get-ns (parser/parse-file-all
                       "test-resources/site/fabricate/example.clj"))))

(defn file->forms
  "Generate a sequence of Clojure form maps from the input file."
  {:malli/schema (m/schema [:=> [:cat [:fn fs/exists?]]
                            [:map [:clojure/forms [:* form-map-schema]]]])}
  [clj-file]
  (let [parsed (parser/parse-file-all clj-file)
        nmspc  (get-ns parsed)]
    {:clojure/forms     (mapv #(node->map %
                                          {:input/file        clj-file
                                           :clojure/namespace nmspc})
                              (:children parsed))
     :clojure/namespace nmspc
     :input/file        clj-file}))

;; think about refactoring these
(defn string->forms
  "Generate a sequence of Clojure form maps from the input string."
  {:malli/schema (m/schema [:-> :string
                            [:map [:clojure/forms [:* form-map-schema]]]])}
  [clj-str]
  (let [parsed (parser/parse-string-all clj-str)
        nmspc  (get-ns parsed)]
    {:clojure/forms     (mapv #(node->map % {:clojure/namespace nmspc})
                              (:children parsed))
     :clojure/namespace nmspc
     :input/string      clj-str}))

(defn- parse-fallback
  [str opts]
  (e/parse-string str
                  (merge {:read-cond :allow :features #{:clj} :all true} opts)))

(defn eval-form
  [{clojure-form :clojure/form
    clj-ns       :clojure/namespace
    clj-str      :clojure/source
    :as          unevaluated-form}]
  (if clojure-form
    (let [calling-ns  *ns*
          start-time  (System/currentTimeMillis)
          eval-output (try {:clojure/result (binding [*ns* (create-ns clj-ns)]
                                              (clojure.core/refer-clojure)
                                              (eval clojure-form))
                            :clojure.eval/duration (- (System/currentTimeMillis)
                                                      start-time)}
                           (catch Exception e
                             #_{:clojure/error (Throwable->map e)}
                             (try (if (string? clj-str)
                                    {:clojure/result (binding [*ns* (create-ns
                                                                     clj-ns)]
                                                       (eval (parse-fallback
                                                              clj-str
                                                              {:auto-resolve
                                                               {:current
                                                                clj-ns}})))
                                     :clojure.eval/duration
                                     (- (System/currentTimeMillis) start-time)
                                     :context        :fallback}
                                    {:clojure/error (Throwable->map e)})
                                  (catch Exception ee
                                    {:clojure/error (Throwable->map ee)
                                     :context       :fallback
                                     :clojure/form  (parse-fallback
                                                     clj-str
                                                     {:auto-resolve
                                                      {:current clj-ns}})}))))]
      (merge unevaluated-form eval-output))
    unevaluated-form))

(comment
  (reader/read-string {:read-cond :allow :features #{:clj}} "#?(:clj :a)")
  read)

;; should there be any "top level" stuff?

;; yes. there should be a map defining the namespace for the file and other
;; metadata.
(defn eval-forms
  "Evaluate the Clojure forms in a parsed file."
  {:malli/schema (m/schema [:-> [:map [:clojure/forms [:* form-map-schema]]]
                            [:map [:clojure/forms [:* form-map-schema]]]])}
  [{:keys [clojure/forms] :as input}]
  (let [evaluated-forms (mapv eval-form forms)]
    (merge input {:clojure/forms evaluated-forms})))

(comment
  (create-ns)
  (ns-name *ns*)
  (kind/md (str (parser/parse-string ";; a comment")))
  (parser/parse-string "^:kindly/hide-code '(quoted-form a b c)")
  (kindly/hide-code "abc")
  (kindly/consider)
  (kin)
  (file->forms *file*))


;; implementation working notes

;; metadata nodes - tricky as usual. reasonable strategy to normalize them
