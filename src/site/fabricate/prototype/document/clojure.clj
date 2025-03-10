(ns site.fabricate.prototype.document.clojure
  "Prototype namespace for generating Hiccup and Kindly values from Clojure forms."
  (:require [site.fabricate.adorn :as adorn]
            [clojure.string :as str]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as zip]
            [rewrite-clj.parser :as parser]
            [malli.core :as m]
            [babashka.fs :as fs]))

;; Refactor: the source -> forms functions from the
;; site.fabricate.prototype.document.clojure ns
;; should be migrated here


;; this means that this namespace has two purposes:
;; 1. generate "semantic" Hiccup from Clojure source code while evaluating it
;; 2. define a way for Clojure source to be tokenized into hiccup for
;; server-side syntax highlighting

(def form-schema
  "Map schema describing a simplified way of storing Clojure forms and their potential results."
  (m/schema
   [:map
    [:clojure/source {:description "Source of the expression as a string"}
     :string]
    [:clojure/form
     {:description "Parsed Clojure expression (unevaluated)" :optional true}
     :any]
    [:clojure.form/metadata {:description "Metadata of form" :optional true}
     :map]
    [:input/file
     {:description "Source file (relative to project)" :optional true}
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

;; comment handling
(def ^:private comment-pattern #"(?:;+\s*)([\V\S]*)(\R*)")

(defn- count-newlines [s] (count (re-seq #"\R" s)))
(defn- ->newlines
  [{:keys [clojure/newlines]}]
  (repeat (count-newlines newlines) [:br {:class "clojure-newline"}]))

(defn- extract-comment-text
  [comment-str]
  (let [[_ comment-txt newlines] (re-matches comment-pattern comment-str)]
    (apply list
           (str/trim comment-txt)
           (->newlines {:clojure/newlines newlines}))))

;; metadata handling

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

;; questions about this:
;; - should this be a multimethod?
;; - should normalization be recursive? should nested metadata also be
;; hidden/converted?
;;    - the answer to this second quetion is "hide only top-level metadata for
;;    now, until it is addressed upstream in rewrite-clj"

;; this function is confused about what it returns and should probably be
;; renamed
;; for clarity because it clashes/overlaps with the function immediately
;; following it!
(defn normalize-node
  "Return a node representing the normalized 'value' for the given node."
  {:malli/schema [:-> [:fn node/node?] [:fn node/node?]]}
  [n]
  (let [t (node/tag n)]
    (if (= t :meta)
      (let [meta-node (first (:children n))
            base-node (peek (:children n))]
        (assoc (normalize-node base-node) :meta (meta-node->metadata-map n)))
      n)))


(defn node->form
  "Convert the given rewrite-clj node into a form map."
  ([n m]
   (let [t        (node/tag n)
         src      (str n)
         src-info (reduce-kv (fn [mm k v]
                               (assoc mm (keyword "clojure.source" (name k)) v))
                             m
                             (meta n))
         r        (case t
                    :comment    {:clojure/comment      (str n)
                                 :clojure.comment/text (extract-comment-text
                                                        (str n))}
                    :newline    {:clojure/newlines (str n) :line-count 0}
                    :meta       (let [n (normalize-node n)]
                                  {:clojure/node     n
                                   :clojure/metadata (:meta n)
                                   :clojure/form     (node/sexpr n)})
                    :whitespace {:clojure/whitespace (str n)}
                    :uneval     {:clojure/uneval (str n)}
                    (if (node/sexpr-able? n)
                      {:clojure/node n :clojure/form (node/sexpr n)}
                      {:clojure/node n}))]
     (merge {:clojure/source src :clojure/node n :node/tag t} m src-info r)))
  ([n] (node->form n {})))


;; denormalization - does it need to be more complicated than this?
(defn- form->meta-node
  [{:keys [clojure/form clojure/source clojure/meta] :as form-map}]
  (parser/parse-string source))

(defn- get-ns
  "Get the namespace symbol from a given multi-form node. Returns the first namespace."
  [node]
  (let [ns-loc (zip/find-value (zip/of-node node) zip/next 'ns)]
    (zip/sexpr (zip/next ns-loc))))

;; reading forms

(defn read-forms
  "Return a vector of Clojure form maps from the input file or string.

If passed a file or string path pointing to an existing file, will read from the file, otherwise treats the input as a string containing Clojure source."
  {:malli/schema (m/schema [:=> [:cat [:or :string [:fn fs/exists?]]]
                            [:map [:clojure/forms [:* form-schema]]]])}
  [clj-src]
  (let [file?    (fs/exists? clj-src)
        parsed   (if file?
                   (parser/parse-file-all clj-src)
                   (parser/parse-string-all clj-src))
        nmspc    (get-ns parsed)
        src-info (merge
                  {:clojure/namespace nmspc}
                  (if file? {:input/file clj-src} {:input/string clj-src}))]
    (merge (select-keys (meta nmspc)
                        [:site.fabricate.document/title
                         :site.fabricate.document/description])
           {:clojure/forms (mapv #(node->form % src-info) (:children parsed))
            :site.fabricate.source/format :clojure/v0}
           src-info)))


;; evaluating forms

(defn- parse-fallback
  [str opts]
  (binding [*read-eval* false]
    (read-string {:read-cond :allow :features #{:clj}} str)))

(defn eval-form
  "Evaluate the Clojure form contained in the given map."
  {:malli/schema (m/schema [:-> form-schema form-schema])}
  [{clojure-form :clojure/form
    clj-ns       :clojure/namespace
    clj-str      :clojure/source
    :or          {clj-ns (ns-name *ns*)}
    :as          unevaluated-form}]
  (if clojure-form
    (let [eval-ns (create-ns (or clj-ns (ns-name *ns*)))
          start-time (System/currentTimeMillis)
          eval-output
          (try
            {:clojure/result        (binding [*ns* eval-ns]
                                      (clojure.core/refer-clojure)
                                      (eval clojure-form))
             :clojure.eval/duration (- (System/currentTimeMillis) start-time)}
            ;; the parse fallback can't be moved to the
            ;; parsing step mostly because it requires
            ;; evaluation to see whether it's failed
            (catch Exception e
              #_{:clojure/error (Throwable->map e)}
              (try (if (string? clj-str)
                     {:clojure/result (binding [*ns* eval-ns]
                                        (eval (parse-fallback clj-str
                                                              {:auto-resolve
                                                               {:current
                                                                (ns-name
                                                                 eval-ns)}})))
                      :clojure.eval/duration (- (System/currentTimeMillis)
                                                start-time)
                      :context        :fallback}
                     {:clojure/error (Throwable->map e)})
                   (catch Exception ee
                     {:clojure/error (Throwable->map ee)
                      :context       :fallback
                      :clojure/form  (parse-fallback
                                      clj-str
                                      {:auto-resolve {:current
                                                      (ns-name eval-ns)}})}))))]
      (merge unevaluated-form eval-output))
    unevaluated-form))

(defn eval-forms
  "Evaluate the Clojure forms in a parsed file."
  {:malli/schema (m/schema [:-> [:map [:clojure/forms [:* form-schema]]]
                            [:map [:clojure/forms [:* form-schema]]]])}
  [{:keys [clojure/forms] :as input}]
  (let [evaluated-forms (mapv eval-form forms)]
    (merge input {:clojure/forms evaluated-forms})))

;; hiccup

;; paragraph detection

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
(defn- trim-newlines [e] (vec (take-while #(not (newline-element? %)) e)))

(defn- code-block
  [{:keys [clojure/result clojure/error clojure/source clojure/metadata
           clojure/node]
    :as   form}]
  (let [hide-code?     (true? (:kindly/hide-code metadata))
        hide-result?   (true? (:kindly/hide-result metadata))
        show-metadata? (false? (:kindly/hide-metadata metadata))
        hiccup?        (= :kind/hiccup (:kindly/kind metadata))
        node           (if show-metadata? (form->meta-node form) node)]
    (cond
      ;; treat hiccup as-is
      hiccup? (list result)
      (and hide-code? hide-result?) '()
      (and result hide-code?) (list [:pre {:class "clojure-result"}
                                     [:code {:class "language-clojure"}
                                      (adorn/clj->hiccup result)]])
      (and result (not hide-result?))
      (list [:pre {:class "clojure-form"}
             [:code {:class "language-clojure"} (adorn/clj->hiccup node)]]
            [:pre {:class "clojure-result"}
             [:code {:class "language-clojure"} (adorn/clj->hiccup result)]])
      (and error (and (not hide-code?) (not hide-result?)))
      (list [:pre {:class "clojure-form"}
             [:code {:class "language-clojure"} (adorn/clj->hiccup node)]]
            [:pre {:class "clojure-error"}
             [:code {:class "language-clojure"} (adorn/clj->hiccup error)]])
      (and error hide-code?) (list [:pre {:class "clojure-error"}
                                    [:code {:class "language-clojure"}
                                     (adorn/clj->hiccup error)]])
      :default (list [:pre {:class "clojure-form"}
                      [:code {:class "language-clojure"}
                       (adorn/clj->hiccup source)]]))))

(defn- new-paragraph
  [{:keys [clojure.comment/text] :as form}]
  (into [:p {:class "clojure-comment"}] text))

(defn- get-element-type
  [hiccup-vector]
  (let [tag (first hiccup-vector)] (if (#{:p :pre} tag) tag :hiccup)))

;; because matching dispatches on two things:
;; 1. the previous element (or attributes derived from it)
;; 2. the next element (or attributes derived from it)
;; I think this could actually be rewritten as a case statement,
;; if the elements are preprocessed correctly

;; the dispatch is complex enough that I'm tempted to reach for a multimethod -
;; but I won't!
(defn- merge-paragraphs
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
                                (vector? prev-element) (get-element-type
                                                        prev-element))
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
      [:p :comment]           (list (into (conj (trim-newlines prev-element)
                                                " ")
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
      [:attr-map :newlines]   (list prev-element)
      [:hiccup :newlines]     (list prev-element)
      [:hiccup :comment]      (list prev-element (new-paragraph next-form))
      [:hiccup :code-block]   (apply list prev-element (code-block next-form))
      ;; uneval always gets discarded
      [:any :uneval]          (list prev-element))))


(defn forms->hiccup
  "Produce a Hiccup vector from the given forms."
  [{:keys [clojure/forms] page-ns :clojure/namespace :as page-map}]
  (let [ns-meta (meta (find-ns page-ns))
        main    (reduce (fn process-next-form [body-hiccup
                                               {:keys [] :as next-form-map}]
                          (let [last-element  (peek body-hiccup)
                                rest-elements (pop body-hiccup)]
                            (into rest-elements
                                  (merge-paragraphs last-element
                                                    next-form-map))))
                        ;; TODO: think of better ways to include metadata +
                        ;; provenance
                        [:main {:data-clojure-namespace page-ns}]
                        forms)]
    main))


;; kindly





(defn form->kind
  "Produce a kindly value from the given form.

By default, produces a plaintext string from a Clojure comment form and marks it as a Clojure comment with metadata. Will wrap values not supporting metadata in vectors."
  ([{:keys       [clojure/result clojure/metadata clojure/form clojure/source]
     clj-comment :clojure/comment
     clj-error   :clojure/error
     :as         evaluated-form}
    ;; options taken from clay as examples
    {:keys [hide-nils hide-vars] :or {hide-nils true hide-vars true} :as opts}]
   (let [kindly-metadata (select-keys metadata
                                      [:kindly/hide-code :kindly/hide-result
                                       :kindly/kind])
         show-metadata?  (false? (:kindly/hide-metadata metadata))
         code            (cond (:kindly/hide-code kindly-metadata) nil
                               (:clojure/whitespace evaluated-form) nil
                               (:clojure/newlines evaluated-form) nil
                               show-metadata? ^{:kindly/kind :kind/code}
                                              [source]
                               :default ^{:kindly/kind :kind/code}
                                        [(str (:clojure/node evaluated-form))])
         result-value    (cond (:kindly/hide-result kindly-metadata) nil
                               (instance? clojure.lang.IObj result)
                               (with-meta result kindly-metadata)
                               clj-comment
                               ^{:kindly/kind :comment}
                               [(str/replace clj-comment #"^;+\s*" "")]
                               clj-error (with-meta clj-error kindly-metadata)
                               (:clojure/whitespace evaluated-form) nil
                               (:clojure/newlines evaluated-form) nil
                               (not (instance? clojure.lang.IObj result))
                               (with-meta [result] kindly-metadata))]
     (cond (= :kind/hiccup (:kindly/kind kindly-metadata)) result-value
           (= :comment (:kindly/kind (meta result-value))) result-value
           (and code result-value) ^{:kindly/kind :kind/fragment}
                                   [code result-value]
           :default (or code result-value))))
  ([form] (form->kind form {})))

(defn forms->fragment
  "Produce a kindly fragment from the given forms.

Consecutive comment forms get combined into a single plaintext string. Whitespace-only forms get skipped.

See https://scicloj.github.io/kindly-noted/kinds.html#fragment
for documentation about the fragment kind."
  [{:keys [clojure/forms] page-ns :clojure/namespace :as page-map}]
  (let [ns-meta (meta (find-ns page-ns))]
    (reduce (fn [fragment next-form]
              (let [prev-kind (peek fragment)
                    next-kind (form->kind next-form)]
                (cond (and (= :comment (:kindly/kind (meta prev-kind)))
                           (= :comment (:kindly/kind (meta next-kind))))
                      (conj (pop fragment)
                            ^{:kindly/kind :comment}
                            [(str (first prev-kind) " " (first next-kind))])
                      (or (nil? next-kind) (:kindly/hide-code (meta next-kind)))
                      fragment
                      :default (conj fragment next-kind))))
            (with-meta
              []
              (merge ns-meta {:kindly/kind :kind/fragment :clojure/ns page-ns}))
            forms)))


(comment
  ;; crazy idea: return hidden DOM elements????
  (-> "test-resources/site/fabricate/example.clj"
      file->forms
      eval-forms
      forms->fragment)
  (scicloj.kindly.v4.api/attach-meta-to-value 2 {:type :test})
  (with-meta [1 2 3] nil))
