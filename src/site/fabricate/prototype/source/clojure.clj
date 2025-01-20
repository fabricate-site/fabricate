(ns site.fabricate.prototype.source.clojure
  "Fabricate namespace defining methods for turning Clojure namespaces into Hiccup documents"
  (:require [hiccup2.core :as hiccup]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as zip]
            [malli.core :as m]
            [babashka.fs :as fs]
            [edamame.core :as e]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [site.fabricate.adorn :as adorn]
            [site.fabricate.api :as api]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]))


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

(def ^:private comment-pattern #"(?:;+\s*)([\s\S]*)(?:\R*)")

(defn- extract-comment-text
  [comment-str]
  (str/trim (last (re-matches comment-pattern comment-str))))

;; should this be a multimethod?
(defn normalize-node
  "Return a map representing the 'value' for the given node"
  [n]
  (let [t      (node/tag n)
        src    (str n)
        result (case t
                 :comment    {:clojure/comment      (str n)
                              :clojure.comment/text (extract-comment-text (str
                                                                           n))}
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
  (binding [*read-eval* false]
    (read-string {:read-cond :allow :features #{:clj}} str)))

(defn eval-form
  "Evaluate the Clojure form contained in the given map."
  {:malli/schema (m/schema [:-> form-map-schema form-map-schema])}
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
                           ;; the parse fallback should be moved to... the
                           ;; parsing step!
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


;; rules:
;; 1. any comments separated by 2 or more newlines are separate paragraphs
;; 2. comments separated by a single newline get combined into a single
;; paragraph, with line breaks removed
;; 3. clojure expressions always terminate a paragraph
;; 4. paragraphs are not grouped together into divs
;; 5. consecutive semicolons are ignored.

;; these functions probably should be in a different namespace
;; and maybe should be a part of a public API; they're not really specific to
;; the Clojure evaluation mode - they're for dealing with Hiccup elements
(defn- paragraph-element? [e] (and (vector? e) (= :p (first e))))
(defn- code-element? [e] (and (vector? e) (= :pre (first e))))
(defn- newline-element? [e] (and (vector? e) (= :br (first e))))
(defn- trailing-newlines [e] (take-while newline-element? (reverse e)))
(defn- count-newlines [s] (count (re-seq #"\R" s)))
(defn- trim-newlines [e] (vec (take-while #(not (newline-element? %)) e)))
(defn- ->newlines
  [{:keys [clojure/newlines]}]
  (repeat (count-newlines newlines) [:br {:class "clojure-newline"}]))

(defn- code-block
  [{:keys [clojure/result clojure/error clojure/source] :as form}]
  (cond result   (list [:pre {:class "clojure-form"}
                        [:code {:class "language-clojure"}
                         (adorn/clj->hiccup source)]]
                       [:pre {:class "clojure-result"}
                        [:code {:class "language-clojure"}
                         (adorn/clj->hiccup result)]])
        error    (list [:pre {:class "clojure-form"}
                        [:code {:class "language-clojure"}
                         (adorn/clj->hiccup source)]]
                       [:pre {:class "clojure-error"}
                        [:code {:class "language-clojure"}
                         (adorn/clj->hiccup error)]])
        :default (list [:pre {:class "clojure-form"}
                        [:code {:class "language-clojure"}
                         (adorn/clj->hiccup source)]])))
(defn- new-paragraph
  [{:keys [clojure.comment/text] :as form}]
  [:p {:class "clojure-comment"} text])

;; because matching dispatches on two things:
;; 1. the previous element (or attributes derived from it)
;; 2. the next element (or attributes derived from it)
;; I think this could actually be rewritten as a case statement,
;; if the elements are preprocessed correctly

;; the dispatch is complex enough that I'm tempted to reach for a multimethod -
;; but I won't!
(defn merge-paragraphs
  "Combine the previous Hiccup form with the next Clojure form map"
  [prev-element next-form]
  (let [prev-element-type (cond (:clojure/uneval next-form) :any
                                (map? prev-element) :attr-map
                                ;; paragraph break: 2+ <br> elements
                                (and (vector? prev-element)
                                     (= :p (first prev-element))
                                     (<= 2
                                         (count (trailing-newlines
                                                 prev-element))))
                                :break
                                (vector? prev-element) (nth prev-element 0))
        next-form-type    (cond (:clojure/uneval next-form) :uneval
                                (contains? next-form :clojure/result)
                                :code-block
                                (:clojure/comment
                                  next-form)
                                :comment
                                (:clojure/newlines next-form) :newlines
                                (:clojure/whitespace next-form) :whitespace
                                (:clojure/error next-form) :code-block
                                :default (throw (ex-info "Unmatched form type"
                                                         {:clojure/form
                                                          next-form})))]
    (case [prev-element-type next-form-type]
      [:break :comment]       (list (trim-newlines prev-element)
                                    (new-paragraph next-form))
      [:break :code-block]    (apply list
                                     (trim-newlines prev-element)
                                     (code-block next-form))
      [:p :comment]           (list (conj (trim-newlines prev-element)
                                          " "
                                          (:clojure.comment/text next-form)))
      [:p :newlines]          (list (into prev-element (->newlines next-form)))
      [:p :whitespace]        (list (into prev-element
                                          (:clojure/whitespace next-form)))
      [:p :code-block]        (apply list
                                     (trim-newlines prev-element)
                                     (code-block next-form))
      [:pre :comment]         (list (trim-newlines prev-element)
                                    (new-paragraph next-form))
      [:pre :code-block]      (apply list prev-element (code-block next-form))
      [:pre :newlines]        (list prev-element)
      [:pre :whitespace]      (list prev-element)
      [:attr-map :comment]    (list prev-element (new-paragraph next-form))
      [:attr-map :code-block] (apply list prev-element (code-block next-form))
      ;; uneval always gets discarded
      [:any :uneval]          (list prev-element))))

(defn forms->hiccup
  "Produce a Hiccup vector from the given forms."
  [{:keys [clojure/forms] :as page-map}]
  (let [main (reduce
              (fn process-next-form [body-hiccup {:keys [] :as next-form-map}]
                (let [last-element  (peek body-hiccup)
                      rest-elements (pop body-hiccup)]
                  (into rest-elements
                        (merge-paragraphs last-element next-form-map))))
              ;; TODO: think of better ways to include metadata +
              ;; provenance
              [:main {:data-clojure-namespace (:clojure/namespace page-map)}]
              forms)]
    [:html [:head [:link {:rel "stylesheet" :href "/reset.css"}]]
     [:body main]]))

(defmethod api/build [:clojure.v0.test :hiccup]
  [{source-location :site.fabricate.source/location :as entry} opts]
  (merge entry
         {:site.fabricate.document/format :hiccup
          :site.fabricate.document/data   (-> source-location
                                              file->forms
                                              forms->hiccup)}))

(comment
  (create-ns)
  (ns-name *ns*)
  (kind/md (str (parser/parse-string ";; a comment")))
  (parser/parse-string "^:kindly/hide-code '(quoted-form a b c)")
  (kindly/hide-code "abc")
  (kindly/consider)
  (kin)
  (file->forms *file*))
