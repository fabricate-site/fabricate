(ns site.fabricate.prototype.read
  "Parsing + evaluation utilities for embedded clojure forms.
  The functions in this namespace split the text into a sequence of Hiccup forms and embedded expressions,
  which is then traversed again to evaluate it, embedding (or not) the results
  of those expressions within the Hiccup document."
  (:require [clojure.edn :as edn]
            [hiccup.util :as util]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.reader :as r]
            [rewrite-clj.zip :as z]
            [rewrite-clj.parser :as p]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.generator :as mg]
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
    [:expr-src {:doc "The source expression as a string"} :string]
    [:expr {:doc "An expression intended to have its results embedded in the resulting text."
            :optional true} :any]
    [:exec {:optional true
            :doc "An expression intended be evaluated for side effects or definitions."} :any]
    [::parse-error {:optional true :doc ""}
     [:map [:type [:fn class?]]
      [:message :string]]]
    [:file {:optional true :doc "The source file of the expression"}
     :string]]))

(def evaluated-expr-schema
  (-> parsed-expr-schema
      (mu/assoc :result :any)
      (mu/assoc :error [:or :nil :map])))

(defn parsed-form->expr-map
  {:malli/schema [:=> [:cat [:schema [:cat :string :string :string]]]
                  parsed-expr-schema]}
  [parsed-form]
  (let [[t form-or-ctrl? form?]  parsed-form
        parse-metadata (meta parsed-form)
        read-results
        (try (r/read-string (or form? form-or-ctrl?))
             (catch Exception e
               {:error
                (let [em (Throwable->map e)]
                  (merge
                   (select-keys (first (:via em)) [:type])
                   (select-keys
                    em
                    [:cause :data])))}))
        m (with-meta
            (merge {:expr-src (if (vector? form-or-ctrl?) form? form-or-ctrl?)
                    :display false}
                   (if (or (:error read-results) (::parse-error read-results))
                     (assoc read-results :expr nil)))
            parse-metadata)]
    (cond
      (::parse-error m) m
      (:error m) m
      (not (vector? form-or-ctrl?))
      (assoc m :exec read-results)
      :else
      (case (second form-or-ctrl?)
        "+" (assoc m :exec read-results :display true)
        "=" (assoc m :expr read-results)
        "+=" (assoc m :expr read-results :display true)))))

(defn extended-form->form
  {:malli/schema [:=> [:cat [:schema [:* :string]]] [:* :any]]}
  [[tag open front-matter [_ & forms] close :as ext-form]]
  (let [delims (str open close)
        parsed-front-matter
        (if (= "" front-matter) '()
            (map (fn [e] {:expr-src (str e) :expr e})
                 (r/read-string (str open front-matter close))))]
    (with-meta (cond (= delims "[]") (apply conj [] (concat parsed-front-matter forms))
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

(comment
  (-> (template "✳=((+ 2 3)🔚")
      (second)
      parsed-form->expr-map))

(defn read-template
  {:malli/schema [:=> [:cat :string] parsed-schema]}
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
  [{:keys [expr-src exec expr error result display]
    :as parsed-expr}]
  (cond
    error [:div [:h6 "Error"]
           [:dl
            [:dt "Error type"]
            [:dd [:code (str (:type error))]]
            [:dt "Error message"]
            [:dd [:code (str (:cause error))]]
            [:dt "Error phase"]
            [:dd [:code (str (:phase error))]]
            [:dt "Location"]
            [:dd (lines->msg (meta parsed-expr))]]
           [:details [:summary "Source expression"]
            [:pre [:code {:class "language-clojure"} expr-src]]]]
    display (list [:pre [:code {:class "language-clojure"}
                         (render-src (or exec expr))]] result)
    :else result))

;; post-validator should have the following signature
;; if it validates, return the input in a map: {:result input}
;; if it doesn't, return a map describing the error

(defn eval-parsed-expr
  {:malli/schema
   [:=> [:cat parsed-expr-schema :boolean [:fn fn?]]
    [:or :map :any]]}
  ([{:keys [expr-src expr exec error result display
            fabricate.read/parse-error]
     :as expr-map} simplify? post-validator]
   (let [evaluated-expr-map
         (try (assoc expr-map
                     :result (eval (or exec expr))
                     :error (or nil error))
              (catch Exception e
                (assoc expr-map
                       :result nil
                       :error (merge {:type (.getClass e)
                                      :message (.getMessage e)}
                                     (select-keys (Throwable->map e)
                                                  [:cause :phase])))))
         res (with-meta evaluated-expr-map (meta expr-map))
         validated (post-validator (:result res))]
     (when (var? (:result res))
       (alter-meta!
        (:result res)
        (fn [var-meta form-meta]
          (-> var-meta
              (merge form-meta)
              (#(assoc % :column (% :instaparse.gll/start-column)
                       :line (% :instaparse.gll/start-line)))))
        (meta expr-map)))
     (cond
       (and (or error (:error res)) simplify?) (form->hiccup res)
       (or error (:error res)) (assoc res :result (form->hiccup res))
       (and simplify? display expr) (form->hiccup res)
       (and exec display simplify?)
       [:pre [:code {:class "language-clojure"} expr-src]]
       (and exec display)
       (assoc (merge expr-map res)
              :result [:pre [:code {:class "language-clojure"} expr-src]])
       (and expr simplify? (:result res)) ; nil is overloaded here
       (:result res)
       (and exec simplify?) nil
       (and (nil? (:result res)) (nil? (:error res)))
       nil
       :else (merge expr-map res))))
  ([expr simplify?] (eval-parsed-expr expr simplify? (constantly true)))
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

(def metadata-schema
  "Malli schema for unevaluated metadata form/map"
  [:catn
   [:def [:= 'def]]
   [:name [:= 'metadata]]
   [:meta-map :map]])

(defn get-metadata
  "Get the metadata form from the parse tree"
  {:malli/schema [:=> [:cat parsed-schema] [:map]]}
  [expr-tree]
  (->> expr-tree
       (tree-seq vector? identity)
       (filter #(and (m/validate parsed-expr-schema %)
                     (m/validate metadata-schema (:exec %))))
       first
       :exec))

(comment
  (m/schema
   [:function
    [:=> [:cat [:sequential :int]] :any]
    [:=> [:cat [:sequential :int] :boolean] :any]])

  (m/schema
   [:function
    [:=> [:cat :boolean [:* :int]] :any]
    [:=> [:cat [:* :int]] :any]])

  (m/validate [:sequential [:cat :int]] [[1 2]])

  (mg/generate [:cat [:schema [:cat [:* :int]]] :boolean])

  (mg/generate [:cat [:schema [:cat [:* :int]]]]))

(defn eval-all
  {:malli/schema
   (m/schema
    [:function
     [:=> [:cat #_[:schema parsed-schema] [:vector :any]] [:vector :any]]
     [:=> [:cat #_[:schema parsed-schema] [:vector :any] :boolean] [:vector :any]]
     [:=> [:cat #_[:schema parsed-schema] [:vector :any] :boolean :symbol]
      [:vector :any]]])}
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
                       [:pre [:code {:class "language-clojure"} source-code]])
         [:pre [:code {:class "language-clojure"} source-code]])))
  ([file-path] (include-source {} file-path)))

(defn include-def
  "Excerpts the source code of the given symbol in the given file."
  {:malli/schema
   [:function
    [:=> [:cat [:map
                [:render-fn {:optional true} [:fn fn?]]
                [:def-syms {:optional true} [:set :symbol]]
                [:container {:optional true} [:vector :any]]]
          :symbol :string]
     [:vector :any]]
    [:=> [:cat :symbol :string] [:vector :any]]]}
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

(defn include-form
  "Return the form in the file matching the predicate as a rewrite-clj node."
  {:malli/schema
   [:=> [:cat :string [:fn fn?]] :any]}
  [src-file pred]
  (let [fzip (z/of-file src-file)]
    (z/node (z/find-next-depth-first fzip #(pred (z/sexpr %))))))

(def file-metadata-schema
  (m/schema
   [:map {:description "File metadata used by fabricate"}
    [:site.fabricate.file/filename {:description "The title of the template file, absent any prefixes"} :string]
    [:site.fabricate.file/output-extension {:description "The extension of the output format of the file"} :string]
    [:site.fabricate.file/template-suffix {:description "The suffix following the output file extension"} :string]
    [:site.fabricate.file/created {:optional true :description "When the file was created"} :string]
    [:site.fabricate.file/modified {:optional true :description "When the file was modified"} :string]]))

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
        [_ fname output-extension suffix]
        (re-matches #"(^.*?)[.]([^.]+)[.]([^.]+)$"
                    (.toString (.getFileName local-f)))]
    {:site.fabricate.file/filename (if (= "" local-subdir) fname (str local-subdir "/" fname))
     :site.fabricate.file/output-extension output-extension
     :site.fabricate.file/template-suffix (str "." suffix)}))

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

(def ^:private parsed-encoder
  (m/encoder parsed-schema
             (mt/transformer {:name :get})))

(defn parse
  {:malli/schema
   [:function
    [:=> [:cat :string
          [:map
           [:start-seq {:optional true} [:vector :any]]
           [:filename {:optional true} :string]]] [:vector :any]]
    [:=> [:cat :string] [:vector :any]]]}
  ([src {:keys [start-seq filename]
         :or {start-seq [] filename ""}}]
   (let [parsed (read-template src)]
     (into start-seq
           (map #(try (with-meta %
                        (merge (meta %) {:file filename}))
                      (catch Exception e %))
                (rest (parsed-encoder parsed))))))
  ([src] (parse src {})))

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
