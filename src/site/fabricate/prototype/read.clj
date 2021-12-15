(ns site.fabricate.prototype.read
  "Parsing + evaluation utilities for embedded clojure forms.
  The functions in this namespace split the text into a sequence of Hiccup forms and embedded expressions,
  which is then traversed again to evaluate it, embedding (or not) the results
  of those expressions within the Hiccup document."
  (:require [clojure.edn :as edn]
            [hiccup.util :as util]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.reader :as r]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.transform :as mt]
            [site.fabricate.prototype.schema :as schema]
            [site.fabricate.prototype.read.grammar :refer [template]]
            [instaparse.core :as insta]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn nil-or-empty?
  {:malli/schema [:=> [:cat :any] :boolean]}
  [v]
  (if (seqable? v) (empty? v)
      (nil? v)))

(defn conj-non-nil
  {:malli/schema [:=> [:cat [:schema [:* :any]] [:* :any]]
                  [:schema [:* :any]]]}
  [s & args]
  (reduce conj s (filter #(not (nil-or-empty? %)) args)))

(defn md5
  {:malli/schema [:=> [:cat :string] :string]}
  [^String s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(def parsed-expr-schema
  (m/schema
   [:map
    [:src {:doc "The source expression as a string"} :string]
    [:expr {:doc "An expression intended to have its results embedded in the resulting text."
            :optional true} :any]
    [:exec {:optional true
            :doc "An expression intended be evaluated for side effects or definitions."} :any]
    [::parse-error {:optional true :doc ""}
     [:map [:type [:fn class?]]
      [:message :string]]]]))

(def evaluated-expr-schema
  (-> parsed-expr-schema
      (mu/assoc :result :any)
      (mu/assoc :error [:or :nil :map])))

(defn parsed-form->expr-map
  {:malli/schema [:=> [:cat [:schema [:cat :string :string :string]]]
                  :map]}
  [parsed-form]
  (let [[t form-or-ctrl? form?]  parsed-form
        parse-metadata (meta parsed-form)
        read-results
        (try (r/read-string (or form? form-or-ctrl?))
             (catch Exception e
               {:err
                (let [em (Throwable->map e)]
                  (merge
                   (select-keys (first (:via em)) [:type])
                   (select-keys
                    em
                    [:cause :data])))}))
        m (with-meta
            (merge {:src (if (vector? form-or-ctrl?) form? form-or-ctrl?)
                    :display false}
                   (if (:err read-results)
                     (assoc read-results :expr nil)))
            parse-metadata)]
    (cond
      (:err m) m
      (not (vector? form-or-ctrl?))
      (assoc m :exec read-results)
      :else
      (case (second form-or-ctrl?)
        "+" (assoc m :exec read-results :display true)
        "=" (assoc m :expr read-results)
        "+=" (assoc m :expr read-results :display true)))))

(defn extended-form->form
  {:malli/schema [:=> [:cat [:schema [:* :string]]] [:* :any]]}
  [[tag open front-matter [_ forms] close :as ext-form]]
  (let [delims (str open close)
        parsed-front-matter
        (if (= "" front-matter) '()
            (map (fn [e] {:src (str e) :expr e})
                 (r/read-string (str open front-matter close))))]
    (with-meta (cond (= delims "[]") (apply conj [] (concat parsed-front-matter [forms]))
                     (= delims "()") (concat () parsed-front-matter forms))
      (meta ext-form))))

(def parsed-schema
  "Malli schema describing the elements of a fabricate template after it has been parsed by the Instaparse grammar"
  (m/schema
   [:schema
    {:registry
     {::txt [:tuple {:encode/get {:leave second}} [:= :txt] :string]
      ::form
      [:or [:tuple {:encode/get {:leave parsed-form->expr-map}}
            [:= :expr] [:tuple [:= :ctrl] [:enum "=" "+" "+="]] :string]
       [:tuple {:encode/get {:leave parsed-form->expr-map}}
        [:= :expr] :string]]
      ::extended-form
      [:tuple
       {:encode/get {:leave extended-form->form}}
       [:= :extended-form]
       [:enum "{" "[" "("]
       :string
       [:cat
        [:= :form-contents]
        [:* [:or [:ref ::txt] [:ref ::form] [:ref ::extended-form]]]]
       [:enum "}" "]" ")"]]}}
    [:cat
     [:= {:encode/get {:leave (constantly nil)}} :template]
     [:*
      [:or
       [:ref ::txt]
       [:ref ::form]
       [:ref ::extended-form]]]]]))

(defn read-template
  {:malli/schema [:=> [:cat :string] parsed-schema]}
  [template-txt]
  (let [attempt (template template-txt)]
    (if (insta/failure? attempt)
      {::parse-failure attempt}
      ;; preserve provenance information, you'll
      ;; never know when it might be useful
      ;;
      ;; for example, cleverly calculating the start and end
      ;; points of a paragraph (after detection) in the source
      ;; template (this may be too clever)
      (insta/add-line-and-column-info-to-metadata template-txt attempt))))

(defn render-src
  {:malli/schema [:=> [:cat :any :boolean] :string]}
  ([src-expr rm-do?]
   (let [exp (if (and rm-do?
                      (seq? src-expr)
                      (= (first src-expr) 'do))
               (second src-expr) src-expr)]
     (util/escape-html (with-out-str (pprint exp)))))
  ([src-expr] (render-src src-expr false)))

(defn- lines->msg [form-meta]
  (let [line-info
        (if (= (:instaparse.gll/start-line form-meta)
               (:instaparse.gll/end-line form-meta))
          (list "Line " [:strong (:instaparse.gll/end-line form-meta)])
          (list "Lines " [:strong (:instaparse.gll/start-line form-meta) "-" (:instaparse.gll/end-line form-meta)]))
        column-info
        (if (= (:instaparse.gll/start-column form-meta)
               (:instaparse.gll/end-column form-meta))
          (list "Column " [:strong (:instaparse.gll/end-column form-meta)])
          (list "Columns " [:strong (:instaparse.gll/start-column form-meta) "-" (:instaparse.gll/end-column form-meta)]))]
    (concat line-info [", "] column-info)))

(def error-form-schema
  [:tuple [:= :div] [:= [:h6 "Error"]]
   [:schema [:cat [:= :dl] [:* :any]]]
   [:schema [:cat [:= :details] [:* :any]]]])

(defn form->hiccup
  "If the form has no errors, return its results.
  Otherwise, create a hiccup form describing the error."
  {:malli/schema [:=> [:cat parsed-expr-schema] [:or error-form-schema :any]]}
  [{:keys [src exec expr err result display]
    :as parsed-expr}]
  (cond
    err [:div [:h6 "Error"]
         [:dl
          [:dt "Error type"]
          [:dd [:code (str (:type err))]]
          [:dt "Error message"]
          [:dd [:code (str (:cause err))]]
          [:dt "Error phase"]
          [:dd [:code (str (:phase err))]]
          [:dt "Location"]
          [:dd (lines->msg (meta parsed-expr))]]
         [:details [:summary "Source expression"]
          [:pre [:code src]]]]
    display (list [:pre [:code (render-src (or exec expr) true)]] result)
    :else result))

;; post-validator should have the following signature
;; if it validates, return the input in a map: {:result input}
;; if it doesn't, return a map describing the error

(defn eval-parsed-expr
  {:malli/schema [:=> [:cat parsed-expr-schema :boolean [:fn fn?]]
                  [:or :map :any]]}
  ([{:keys [src expr exec err result display]
     :as expr-map} simplify? post-validator]
   (let [res
         (with-meta (if err expr-map
                        (try
                          (assoc
                           expr-map
                           :result
                           (if exec
                             (do (eval exec) nil)
                             (eval expr))
                           :err nil)
                          (catch Exception e
                            (assoc expr-map
                                   :result nil
                                   :err (merge {:type (.getClass e)
                                                :message (.getMessage e)}
                                               (select-keys (Throwable->map e)
                                                            [:cause :phase]))))))
           (meta expr-map))
         validated (post-validator res)]
     (cond
       (and (or err (:err res)) simplify?) (form->hiccup res)
       (or err (:err res)) (assoc res :result (form->hiccup res))
       (and display simplify?)
       [:pre [:code {:class "language-clojure"} src]]
       display
       (assoc (merge expr-map res)
              :result [:pre [:code {:class "language-clojure"} src]])
       (and simplify? (:result res))    ; nil is overloaded here
       (:result res)
       (and (nil? (:result res)) (nil? (:err res)))
       nil
       :else (merge expr-map res))))
  ([expr simplify?] (eval-parsed-expr expr simplify? (fn [e] {:result e})))
  ([expr] (eval-parsed-expr expr false)))

(defn yank-ns
  "Pulls the namespace out of the first expression in the parse tree."
  {:malli/schema
   [:=> [:cat parsed-schema] [:or [:sequential :any] :symbol]]}
  [expr-tree]
  (let [first-expr (->> expr-tree
                        (tree-seq vector? identity)
                        (filter #(m/validate parsed-expr-schema %))
                        first
                        :exec)]
    (if (and (seq? first-expr)
             (schema/ns-form? first-expr))
      (second first-expr)
      nil)))

;; An alternative design choice: rather than making the ns form
;; special and required as the first fabricate form, make the metadata map
;; special and just put the unevaluated ns form within it.

(def metadata-model
  "Malli schema for unevaluated metadata form/map"
  [:catn
   [:def [:= 'def]]
   [:name [:= 'metadata]]
   [:meta-map map?]])

(def metadata-expr-model
  [:catn
   [:do [:= 'do]]
   [:metadata-form [:schema metadata-model]]
   [:nil nil?]])

(defn get-metadata
  "Get the metadata form from the parse tree"
  {:malli/schema [:=> [:cat parsed-schema] [:map]]}
  [expr-tree]
  (->> expr-tree
       (tree-seq vector? identity)
       (filter #(and (m/validate parsed-expr-schema %)
                     (m/validate metadata-model (:exec %))))
       first
       :exec))

(defn eval-all
  {:malli/schema
   [:=> [:cat parsed-schema [:? :boolean] [:? :symbol]]
    [:vector :any]]}
  ([parsed-form simplify? nmspc]
   (let [form-nmspc (yank-ns parsed-form)
         nmspc (if form-nmspc (create-ns form-nmspc) *ns*)]
     (binding [*ns* nmspc]
       (refer-clojure)
       (clojure.walk/postwalk
        (fn [i] (if (m/validate parsed-expr-schema i)
                  (eval-parsed-expr i simplify?)
                  i))
        parsed-form))))
  ([parsed-form simplify?] (eval-all parsed-form simplify? *ns*))
  ([parsed-form] (eval-all parsed-form true)))

(defn include-source
  {:malli/schema [:=> [:cat :map :string] [:vector :any]]}
  ([{:keys [details]
     :or {details nil}
     :as opts} file-path]
   (let [source-code (slurp file-path)]
     (if details (conj [:details [:summary details]]
                       [:pre [:code source-code]])
         [:pre [:code source-code]])))
  ([file-path] (include-source {} file-path)))

(defn include-def
  "Excerpts the source code of the given symbol in the given file."
  {:malli/schema [:=> [:cat [:? [:map [:render-fn [:fn fn?]]
                                 [:def-syms [:set :symbol]]
                                 [:container [:vector :any]]]]
                       :symbol :string]
                  [:vector :any]]}
  ([{:keys [render-fn def-syms container]
     :or {render-fn render-src
          def-syms #{'def 'defn}
          container [:pre [:code {:class "language-clojure"}]]}} sym f]
   (with-open [r (clojure.java.io/reader f)]
     (loop [source (java.io.PushbackReader. r)]
       (if (not (.ready source)) :not-found
           (let [e (try (r/read source)
                        (catch Exception e nil))]
             (if (and (list? e)
                      (def-syms (first e))
                      (= sym (symbol (second e))))
               (conj container (render-fn e))
               (recur source)))))))
  ([sym f] (include-def {} sym f)))

(def file-metadata-schema
  (m/schema
   [:map {:description "File metadata used by fabricate"}
    [:filename {:description "The title of the file, absent any prefixes"} :string]
    [:file-extension {:description "The extension of the output format of the file"} :string]
    [:fabricate/suffix {:description "The suffix following the output file extension"} :string]
    [:created {:optional true :description "When the file was created"} :string]
    [:modified {:optional true :description "When the file was modified"} :string]]))

(defn get-file-metadata
  {:malli/schema [:=> [:cat :string] file-metadata-schema]}
  [file-path]
  (let [wd (-> "."
               io/file
               .toPath)
        local-f (->> file-path io/file
                     .toPath
                     (.relativize wd))
        local-subdir
        (let [li (string/last-index-of (.toString local-f) "/")]
          (if (nil? li)
            ""
            (subs (.toString local-f) 0
                  li)))
        [fname output-extension suffix]
        (string/split (.toString (.getFileName local-f)) (re-pattern "\\."))]
    {:filename (if (= "" local-subdir) fname (str local-subdir "/" fname))
     :file-extension output-extension
     :fabricate/suffix (str "." suffix)}))

(defn ->dir-local-path
  {:malli/schema [:=> [:cat [:fn #(.isInstance java.io.File %)]]
                  :any]}
  [file]
  (.toString
   (.relativize
    (-> (System/getProperty "user.dir")
        io/file
        .getCanonicalFile
        .toPath
        .toAbsolutePath)
    (-> file
        .getCanonicalFile
        .toPath
        .toAbsolutePath))))

(defn parse
  {:malli/schema [:=> [:cat :string [:? [:vector :any]]]
                  [:vector :any]]}
  ([src start-seq]
   (let [parsed (read-template src)]
     (into []
           (rest (m/encode parsed-schema parsed
                           (mt/transformer {:name :get}))))))
  ([src] (parse src [])))

(comment
  (meta (second (read-template "✳=(+ 2 3)🔚")))

  (parsed-form->expr-map (second (read-template "✳=(+ 2 3)🔚")))

  (meta (first (parse "✳=(+ 2 3)🔚")))

  (meta (m/decode
         [:schema {:decode/get {:enter identity}} [:cat [:= :start] :map]]
         (with-meta [:start {:a 2}] {:meta true})
         (mt/transformer {:name :get})))

  (meta (m/encode
         [:cat {:encode/get {:enter identity}} [:= :start] :map]
         (with-meta [:start {:a 2}] {:meta true})
         (mt/transformer {:name :get})))

  (meta (m/encode
         [:tuple {:encode/get {:enter identity}} [:= :start] :map]
         (with-meta [:start {:a 2}] {:meta true})
         (mt/transformer {:name :get})))

  (m/validate [:tuple :keyword [:* :int]] [:k [2]])

  (meta (identity (with-meta [:start {:a 2}] {:meta true}))))
