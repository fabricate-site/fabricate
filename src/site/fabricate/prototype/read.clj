(ns site.fabricate.prototype.read
  "Parsing + evaluation utilities for embedded Clojure forms.
  The functions in this namespace split the text into a sequence of Hiccup forms and embedded expressions,
  which is then traversed again to evaluate it, embedding (or not) the results
  of those expressions within the Hiccup document."
  (:require [clojure.edn :as edn]
            [hiccup.util :as util]
            [rewrite-clj.zip :as z]
            [rewrite-clj.parser :as p]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.generator :as mg]
            [malli.transform :as mt]
            [site.fabricate.adorn :as adorn]
            [site.fabricate.prototype.schema :as schema]
            [site.fabricate.prototype.kindly :as kindly]
            [site.fabricate.prototype.eval :as prototype.eval]
            [site.fabricate.prototype.read.grammar :as grammar :refer
             [template]]
            [instaparse.core :as insta]
            [instaparse.combinators :as combo]
            [clojure.string :as string]
            [clojure.java.io :as io]))

#_(def parsed-expr-schema
    "Schema describing the map used by Fabricate to evaluate forms embedded within page templates."
    (m/schema
     [:map [:expr-src {:doc "The source expression as a string"} :string]
      [:expr
       {:doc
        "An expression intended to have its results embedded in the resulting text."
        :optional true} :any]
      [:exec
       {:optional true
        :doc
        "An expression intended be evaluated for side effects or definitions."}
       :any]
      [::parse-error {:optional true :doc ""}
       [:map [:type [:fn class?]] [:message :string]]]
      [:file {:optional true :doc "The source file of the expression"}
       :string]]))

;; the :expr and :exec keywords need to be replaced by :kindly/hide-code and
;; :kindly/hide-result where appropriate

(def parsed-expr-schema
  (-> site.fabricate.prototype.kindly/Form
      (mu/merge
       [:map
        [::parse-error
         {:optional true
          :description
          "Error message returned when Fabricate enounters a parsing error."}
         :map]
        [:error
         {:optional    true
          :description "Error map describing error thrown by source expression"}
         #_:map [:or :map :nil]]])
      ;; TODO set default kind
      (mu/optional-keys [:value :kind])
      (mu/update-properties
       assoc
       :description
       "Schema for Clojure expressions parsed from Fabricate templates")))


(def ^{:malli/schema [:=> [:cat :any] :boolean]} fabricate-expr?
  "Returns true if the given value matches the schema for parsed Fabricate expressions."
  (m/validator parsed-expr-schema))


(def ^:private read-str (comp z/sexpr z/of-string))

(comment
  (template "✳=(+ 3 4)🔚")
  (template)
  (let [enlive-parser (insta/parser grammar/grammar :output-format :enlive)]
    (doseq [e ["✳=(+ 3 4)🔚" "✳(+ 3 4)🔚"]]
      (println (template e))
      (println (enlive-parser e)))))

(defn parsed-form->expr-map
  "Transforms the results of a parsed Fabricate expression into the map used for evaluation."
  {:malli/schema [:=> [:cat [:schema [:cat :string :string :string]]]
                  parsed-expr-schema]}
  [parsed-form]
  (let [[t form-or-ctrl? form?] parsed-form
        parse-metadata (meta parsed-form)
        read-results (try (read-str (or form? form-or-ctrl?))
                          (catch Exception e
                            {:error (let [em (Throwable->map e)]
                                      (merge
                                       (select-keys (first (:via em)) [:type])
                                       (select-keys em [:cause :data])))}))
        m (with-meta
            (merge {:code (if (vector? form-or-ctrl?) form? form-or-ctrl?)
                    :kindly/hide-code true
                    :kindly/hide-value true}
                   (if (or (:error read-results) (::parse-error read-results))
                     (assoc read-results :form nil)
                     {:form read-results}))
            parse-metadata)]
    (cond (::parse-error m) m
          (:error m)        m
          (not (vector? form-or-ctrl?)) m
          :else             (case (second form-or-ctrl?)
                              "+"  (assoc m :kindly/hide-code false)
                              "="  (assoc m :kindly/hide-value false)
                              "+=" (assoc m
                                          :kindly/hide-code  false
                                          :kindly/hide-value false)))))

(defn extended-form->form
  "Converts the parsed grammar describing an extended form to a Hiccup form."
  {:malli/schema [:=> [:cat [:schema [:* :string]]] [:* :any]]}
  [[tag open front-matter [_ & forms] close :as ext-form]]
  (let [delims (str open close)
        parsed-front-matter (if (= "" front-matter)
                              '()
                              (map (fn [e] {:code (str e) :form e})
                                   (read-str (str open front-matter close))))]
    (with-meta (cond (= delims "[]") (into []
                                           (concat parsed-front-matter forms))
                     (= delims "()") (concat () parsed-front-matter forms))
               (meta ext-form))))

(def template-schema
  "Malli schema describing the elements of a fabricate template after it has been parsed by the Instaparse grammar."
  (m/schema [:schema
             {:registry
              {::txt  [:tuple {:encode/get {:leave second}} [:= :txt] :string]
               ::form [:or {:encode/get {:leave parsed-form->expr-map}}
                       [:tuple [:= :expr]
                        [:tuple [:= :ctrl] [:enum "=" "+" "+="]] :string]
                       [:tuple [:= :expr] :string]]
               ::extended-form
               [:tuple {:encode/get {:leave extended-form->form}}
                [:= :extended-form] [:enum "{" "[" "("] :string
                [:cat [:= :form-contents]
                 [:* [:or [:ref ::txt] [:ref ::form] [:ref ::extended-form]]]]
                [:enum "}" "]" ")"]]}}
             [:cat [:= {:encode/get {:leave (constantly nil)}} :template]
              [:* [:or [:ref ::txt] [:ref ::form] [:ref ::extended-form]]]]]))



(defn read-template
  "Parses the given template and adds line and column metadata to the forms."
  {:malli/schema [:=> [:cat :string] template-schema]}
  [template-txt]
  (let [attempt (template template-txt)]
    (if (insta/failure? attempt)
      {::parse-error attempt}
      ;; preserve provenance information, you'll
      ;; never know when it might be useful
      ;;
      ;; for example, cleverly calculating the start and end
      ;; points of a paragraph (after detection) in the source
      ;; template (this may be too clever)
      (insta/add-line-and-column-info-to-metadata template-txt attempt))))


(defn- lines->msg
  [form-meta]
  (let [line-info   (if (= (:instaparse.gll/start-line form-meta)
                           (:instaparse.gll/end-line form-meta))
                      (list "Line "
                            [:strong (:instaparse.gll/end-line form-meta)])
                      (list "Lines "
                            [:strong (:instaparse.gll/start-line form-meta) "-"
                             (:instaparse.gll/end-line form-meta)]))
        column-info (if (= (:instaparse.gll/start-column form-meta)
                           (:instaparse.gll/end-column form-meta))
                      (list "Column "
                            [:strong (:instaparse.gll/end-column form-meta)])
                      (list "Columns "
                            [:strong (:instaparse.gll/start-column form-meta)
                             "-" (:instaparse.gll/end-column form-meta)]))]
    (concat line-info [", "] column-info)))

(def error-form-schema
  "Malli schema describing Hiccup forms that contain error messages"
  (m/schema [:tuple [:= :div] [:maybe [:map [:class [:= "fabricate-error"]]]]
             [:= [:h6 "Error"]] [:schema [:cat [:= :dl] [:* :any]]]
             [:schema [:cat [:= :details] [:* :any]]]]))

;; TODO: this probably needs to be made more robust
(defn read-error?
  "Returns true if the given expression failed to read into a valid Clojure form."
  {:malli/schema [:=> [:cat parsed-expr-schema] :boolean]}
  [error-form]
  (= "Unexpected EOF." (get-in error-form [:error :data :msg])))

;; TODO: should this be a multimethod?
(defn error->hiccup
  "Return a Hiccup form with context for the error."
  {:malli/schema [:=> [:cat parsed-expr-schema] error-form-schema]}
  [{:keys [expr-src exec expr error result display] :as parsed-expr}]
  [:div {:class "fabricate-error"} [:h6 "Error"]
   [:dl {:class "fabricate-error-info"} [:dt "Error type"]
    [:dd {:class "fabricate-error-type"} [:code (str (:type error))]]
    [:dt "Error message"]
    [:dd {:class "fabricate-error-msg"} [:code (str (:cause error))]]
    [:dt "Error phase"]
    [:dd {:class "fabricate-error-phase"} [:code (str (:phase error))]]
    [:dt "Location"]
    [:dd {:class "fabricate-error-location"} (lines->msg (meta parsed-expr))]]
   [:details [:summary "Source expression"]
    [:pre {:class "fabricate-error-src"}
     [:code {:class "language-clojure"}
      (if (read-error? parsed-expr) expr-src (adorn/clj->hiccup expr-src))]]]])

(comment
  (read-error? (first (parse "✳((+ 2 3)🔚")))
  (adorn/form->hiccup "((+ 3 4)"))

;; TODO: should this be a multimethod?
;; a multimethod _implementation_ of site.fabricate.adorn/clj->hiccup?
(defn form->hiccup
  "If the form has no errors, return its results.
  Otherwise, create a hiccup form describing the error."
  {:malli/schema [:=> [:cat parsed-expr-schema] [:or error-form-schema :any]]}
  [{:keys [expr-src exec expr error result display] :as parsed-expr}]
  (cond error   (error->hiccup parsed-expr)
        display (list [:pre
                       [:code {:class "language-clojure"}
                        ;; TODO: make this configurable via multimethod
                        (adorn/clj->hiccup (or exec expr-src))]]
                      result)
        :else   result))

;; post-validator should have the following signature
;; if it validates, return the input in a map: {:result input}
;; if it doesn't, return a map describing the error

;; should this just use the/a clojure namespace?

#_(defn eval-parsed-expr
    "Evaluates the given expression form. Returns the value of the evaluated expression by default. Can optionally return a map with the value and also perform post-validation on the resulting value."
    {:malli/schema [:=> [:cat parsed-expr-schema :boolean [:fn fn?]]
                    [:or :map :any]]}
    ([{:keys [expr-src expr exec error result display
              fabricate.read/parse-error]
       :as   expr-map} simplify? post-validator]
     (let [form-meta (meta expr-map)
           evaluated-expr-map
           (try (assoc expr-map
                       :result (eval (or exec expr))
                       :error  (or nil error))
                (catch Exception e
                  (assoc expr-map
                         :result nil
                         :error  (merge {:type    (.getClass e)
                                         :message (.getMessage e)}
                                        (select-keys (Throwable->map e)
                                                     [:cause :phase])))))
           res       (with-meta evaluated-expr-map (meta expr-map))
           validated (post-validator (:result res))]
       (when (var? (:result res))
         (alter-meta! (:result res)
                      (fn [var-meta form-meta]
                        (-> var-meta
                            (merge form-meta)
                            (#(assoc %
                                     :column (% :instaparse.gll/start-column)
                                     :line   (% :instaparse.gll/start-line)))))
                      (meta expr-map)))
       (cond (and (or error (:error res)) simplify?) (form->hiccup res)
             (or error (:error res)) (assoc res :result (form->hiccup res))
             (and simplify? display expr) (form->hiccup res)
             (and exec display simplify?)
             [:pre [:code {:class "language-clojure"} expr-src]]
             (and exec display)
             (assoc (merge expr-map res)
                    :result
                    [:pre [:code {:class "language-clojure"} expr-src]])
             (and expr simplify? (:result res)) ; nil is overloaded here
             (:result res)
             (and exec simplify?) nil
             (and (nil? (:result res)) (nil? (:error res))) nil
             :else (merge expr-map res))))
    ([expr simplify?] (eval-parsed-expr expr simplify? (constantly true)))
    ([expr] (eval-parsed-expr expr false)))

(defn yank-ns
  "Pulls the namespace form out of the first expression in the parse tree."
  {:malli/schema [:=> [:cat #_template-schema :any]
                  [:or [:sequential :any] :symbol]]}
  [expr-tree]
  (let [first-expr (->> expr-tree
                        (tree-seq vector? identity)
                        (filter #(m/validate parsed-expr-schema %))
                        first
                        :form)]
    (if (and (seq? first-expr) (schema/ns-form? first-expr))
      (second first-expr)
      nil)))

(def metadata-schema
  "Malli schema for unevaluated metadata form/map"
  (m/schema [:catn [:def [:= 'def]] [:name [:= 'metadata]] [:meta-map :map]]))

(defn get-metadata
  "Get the metadata form from the parse tree."
  {:malli/schema [:=> [:cat template-schema] [:map]]}
  [expr-tree]
  (->> expr-tree
       (tree-seq vector? identity)
       (filter #(and (m/validate parsed-expr-schema %)
                     (m/validate metadata-schema (:form %))))
       first
       :form))

(comment
  (m/schema [:function [:=> [:cat [:sequential :int]] :any]
             [:=> [:cat [:sequential :int] :boolean] :any]])
  (m/schema [:function [:=> [:cat :boolean [:* :int]] :any]
             [:=> [:cat [:* :int]] :any]])
  (m/validate [:sequential [:cat :int]] [[1 2]])
  (mg/generate [:cat [:schema [:cat [:* :int]]] :boolean])
  (mg/generate [:cat [:schema [:cat [:* :int]]]]))

(defn- process-form?
  [i]
  (if (fabricate-expr? i)
    (prototype.eval/eval-form i)
    #_(let [evaluated (prototype.eval/eval-form i)]
        (if-not (:kindly/hide-result evaluated)
          (api/display-form evaluated {:site.fabricate.document/format :hiccup})
          ()))
    i))

(defn eval-all
  "Walks the parsed template and evaluates all the embedded expressions within it. Returns a Hiccup form."
  {:malli/schema
   (m/schema
    [:function
     [:=> [:cat #_[:schema template-schema] [:vector :any]] [:vector :any]]
     [:=> [:cat #_[:schema template-schema] [:vector :any] :boolean]
      [:vector :any]]
     [:=> [:cat #_[:schema template-schema] [:vector :any] :boolean :symbol]
      [:vector :any]]])}
  ([parsed-form simplify? nmspc]
   (let [form-nmspc (or (yank-ns parsed-form) nmspc)
         nmspc      (if form-nmspc (create-ns form-nmspc) *ns*)]
     (binding [*ns* nmspc]
       (refer-clojure)
       (let [final-form (clojure.walk/postwalk process-form? parsed-form)]
         (with-meta final-form
                    {:namespace nmspc
                     :metadata  (when-let [m (ns-resolve *ns* 'metadata)]
                                  (var-get m))})))))
  ([parsed-form simplify?] (eval-all parsed-form simplify? nil))
  ([parsed-form] (eval-all parsed-form true)))



(def file-metadata-schema
  "Schema describing a map of file metadata used by fabricate"
  (m/schema
   [:map {:description "File metadata used by fabricate"}
    [:site.fabricate.file/input-filename
     {:description "The title of the template file, absent any prefixes"}
     :string]
    [:site.fabricate.file/output-extension
     {:description "The extension of the output format of the file"} :string]
    [:site.fabricate.file/template-suffix
     {:description "The suffix following the output file extension"} :string]
    [:site.fabricate.file/created
     {:optional true :description "When the file was created"} :string]
    [:site.fabricate.file/modified
     {:optional true :description "When the file was modified"} :string]]))

;; TODO: use babashka.fs for this
(defn get-file-metadata
  "Get the metadata of the file used by Fabricate."
  {:malli/schema [:=> [:cat :string] file-metadata-schema]}
  [file-path]
  (let [wd           (-> "."
                         io/file
                         .toPath)
        local-f      (->> file-path
                          io/file
                          .toPath
                          (.relativize wd))
        local-subdir (let [li (string/last-index-of (.toString local-f) "/")]
                       (if (nil? li) "" (subs (.toString local-f) 0 li)))
        [_ fname output-extension suffix] (re-matches
                                           #"(^.*?)[.]([^.]+)[.]([^.]+)$"
                                           (.toString (.getFileName local-f)))]
    {:site.fabricate.file/input-filename   (if (= "" local-subdir)
                                             fname
                                             (str local-subdir "/" fname))
     :site.fabricate.file/output-extension output-extension
     :site.fabricate.file/template-suffix  (str "." suffix)}))


(def ^:private template-encoder
  (m/encoder template-schema (mt/transformer {:name :get})))

(defn parse
  "Parses the template into a Hiccup expression with unevaluated forms."
  {:malli/schema [:function
                  [:=>
                   [:cat :string
                    [:map [:start-seq {:optional true} [:vector :any]]
                     [:filename {:optional true} :string]]] [:vector :any]]
                  [:=> [:cat :string] [:vector :any]]]}
  ([src {:keys [start-seq filename] :or {start-seq [] filename ""}}]
   (let [parsed (read-template src)]
     (into start-seq
           (map #(try (with-meta % (merge (meta %) {:file filename}))
                      (catch Exception e %))
                (rest (template-encoder parsed))))))
  ([src] (parse src {})))

(comment
  (meta (second (read-template "✳=(+ 2 3)🔚")))
  (parsed-form->expr-map (second (read-template "✳=(+ 2 3)🔚")))
  (meta (first (parse "✳=(+ 2 3)🔚")))
  (meta (m/decode [:schema {:decode/get {:enter identity}}
                   [:cat [:= :start] :map]]
                  (with-meta [:start {:a 2}] {:meta true})
                  (mt/transformer {:name :get})))
  (meta (m/encode [:cat {:encode/get {:enter identity}} [:= :start] :map]
                  (with-meta [:start {:a 2}] {:meta true})
                  (mt/transformer {:name :get})))
  (meta (m/encode [:tuple {:encode/get {:enter identity}} [:= :start] :map]
                  (with-meta [:start {:a 2}] {:meta true})
                  (mt/transformer {:name :get})))
  (m/validate [:tuple :keyword [:* :int]] [:k [2]])
  (m/validate [:tuple [:= :div] [:maybe [:map [:class [:= "fabricate-error"]]]]]
              [:div {:class "fabricate-error"}])
  (malli.error/humanize
   (m/explain error-form-schema
              [:div {:class "fabricate-error"} [:h6 "Error"]
               [:dl [:dt "Error type"]
                [:dd [:code "clojure.lang.ExceptionInfo"]] [:dt "Error message"]
                [:dd [:code "Unexpected EOF while reading item 1 of list."]]
                [:dt "Error phase"] [:dd [:code ""]] [:dt "Location"]
                [:dd '("Line " [:strong 1] ", " "Columns " [:strong 1 "-" 12])]]
               [:details [:summary "Source expression"]
                [:pre [:code "((+ 2 3)"]]]]))
  (meta (identity (with-meta [:start {:a 2}] {:meta true}))))
