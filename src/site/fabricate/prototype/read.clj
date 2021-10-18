(ns site.fabricate.prototype.read
  "Parsing + evaluation utilities for embedded clojure forms.
  The functions in this namespace split the text into a sequence of Hiccup forms and embedded expressions,
  which is then traversed again to evaluate it, embedding (or not) the results
  of those expressions within the Hiccup document."
  {:license {:source "https://github.com/weavejester/comb"
             :type "Eclipse Public License, v1.0"}}
  (:require [clojure.edn :as edn]
            [hiccup.util :as util]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.reader :as r]
            [malli.core :as m]
            [site.fabricate.prototype.schema :as schema]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def delimiters ["✳" "🔚"])

(def delimiters-2
  "Compositional delimiters: the :display emoji can be added to either add or run so that the input is displayed"
  {:add "➕"
   :run "▶"
   :display "👁️"
   :end "⏹"})

;; regex adapted from comb; licensed under EPLv2
(def parser-regex
  (re-pattern
   (str "(?s)\\A"
        "(?:" "(.*?)"
        (first delimiters) "(.*?)" (last delimiters)
        ")?"
        "(.*)\\z")))

(defn get-parser-regex [start end]
  (re-pattern
   (str "(?s)\\A"
        "(?:" "(.*?)"
        start "(.*?)" end
        ")?"
        "(.*)\\z")))

(def ^{:doc "Malli schema for parser regex."} src-model [:re parser-regex])

(comment
  (m/validate src-model "✳(+ 3 4)🔚"))

(defn eval-expr [expr]
  (if (.startsWith expr "=")
    (try
      (eval (r/read-string (subs expr 1)))
      (catch Exception e
        (do (println "caught an exception while reading:" (.getMessage e))
            ::parse-error)))
    (let [res (try (eval (r/read-string expr))
                   (catch Exception e
                     (do (println "caught an exception while reading:" (.getMessage e))
                         ::parse-error)))]
      (if (= res ::parse-error)
        res
        nil))))

(defn yield-expr [expr]
  (let [display? (.startsWith expr "+")
        e (if display? (subs expr 1) expr)
        attempt
        (try {:success
              (cond
                (.startsWith e "=") (r/read-string (subs e 1))
                :else `(do ~(r/read-string e) nil))}
             (catch Exception ex
               (let [{:keys [cause phase]} (Throwable->map ex)]
                 {:error {:type (.getClass ex)
                          :cause cause
                          :phase phase
                          :message (.getMessage ex)}})))]
    {:expr (:success attempt)
     :src (str (first delimiters) expr (last delimiters))
     :err (:error attempt)
     :result nil
     :display display?}))

(defn nil-or-empty? [v]
  (if (seqable? v) (empty? v)
      (nil? v)))

(defn conj-non-nil [s & args]
  (reduce conj s (filter #(not (nil-or-empty? %)) args)))

(defn md5 [^String s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(def ^{:doc "Model for parsed expressions."} expr-model
  [:orn
   [:yield {:doc "An expression intended to have its results embedded in the resulting text."} [:any]]
   [:exec {:doc "An expression intended be evaluated for side effects or definitions."} [:any]]
   [:nil {:doc "An invalid expression"} nil?]])

(comment
  (m/validate expr-model '(+ 3 4)))

(def parsed-expr-model
  [:map
   [:src {:doc (-> src-model var meta :doc)} src-model]
   [:expr {:doc (-> expr-model var meta :doc)} expr-model]
   [::parse-error {:optional true :doc ""}
    [:map [:type [:fn class?]]
     [:message [:string]]]]])

(def evaluated-expr-model
  [:map
   [:src expr-model]
   [:expr ]])

(defn parse
  ([src start-seq]
   (loop [src src form start-seq]
     (let [[_ before expr after] (re-matches parser-regex src)]
       (if expr
         (recur
          after
          (conj-non-nil form before (yield-expr expr)))
         (conj-non-nil form after)))))
  ([src] (parse src [])))

;; post-validator should have the following signature
;; if it validates, return the input in a map: {:result input}
;; if it doesn't, return a map describing the error

(defn eval-parsed-expr
  ([{:keys [src expr err result display]
     :as expr-map} simplify? post-validator]
   (cond err expr-map
         result result
         :else
         (let [res (try
                     {:result (eval expr)}
                     (catch Exception e
                       {:err (merge {:type (.getClass e)
                                     :message (.getMessage e)}
                                    (select-keys (Throwable->map e)
                                                 [:cause :phase]))}))
               validated (post-validator res)]
           (cond
             display
             (merge expr-map res)
             (and simplify? (:result res)) ; nil is overloaded here
             (:result res)
             (and (nil? (:result res)) (nil? (:err res)))
             nil
             :else (merge expr-map res)))))
  ([expr simplify?] (eval-parsed-expr expr simplify? (fn [e] {:result e})))
  ([expr] (eval-parsed-expr expr false)))

(defn yank-ns
  "Pulls the namespace out of the first expression in the parse tree."
  [expr-tree]
  (let [first-expr (->> expr-tree
                        (tree-seq vector? identity)
                        (filter #(m/validate parsed-expr-model %))
                        first
                        :expr)]
    (if (and (seq? first-expr)
             (schema/ns-form? (second first-expr)))
        (second (second first-expr))
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
  [expr-tree]
  (->> expr-tree
       (tree-seq vector? identity)
       (filter #(and (m/validate parsed-expr-model %)
                     (m/validate metadata-expr-model (:expr %))))
       first
       :expr
       second))

(defn eval-all
  ([parsed-form simplify?]
   (let [form-nmspc (yank-ns parsed-form)
         nmspc (if form-nmspc (create-ns form-nmspc) *ns*)]
     (binding [*ns* nmspc]
       (refer-clojure)
       (clojure.walk/postwalk
        (fn [i] (if (m/validate parsed-expr-model i)
                  (eval-parsed-expr i simplify?)
                  i))
        parsed-form))))
  ([parsed-form] (eval-all parsed-form true)))

(defn render-src
  ([src-expr rm-do?]
   (let [exp (if (and rm-do?
                      (seq? src-expr)
                      (= (first src-expr) 'do))
               (second src-expr) src-expr)]
     (util/escape-html (with-out-str (pprint exp)))))
  ([src-expr] (render-src src-expr false)))

(defn form->hiccup
  "If the form has no errors, return its results.
  Otherwise, create a hiccup form describing the error."
  [{:keys [src expr err result display]
    :as parsed-expr}]
  (cond
    err [:div [:h6 "Error"]
         [:dl
          [:dt "Error type"]
          [:dd [:code (str (:type err))]]
          [:dt "Error message"]
          [:dd [:code (str (:cause err))]]
          [:dt "Error phase"]
          [:dd [:code (str (:phase err))]]]
         [:details [:summary "Source expression"]
          [:pre [:code src]]]]
    display (list [:pre [:code (render-src expr true)]] result)
    :else result))

(defn eval-with-errors
  ([parsed-form form-nmspc post-validator]
   (if (symbol? form-nmspc)
     (binding [*ns* (create-ns form-nmspc)]
       (refer-clojure)
       (clojure.walk/postwalk
        (fn [i] (if (m/validate parsed-expr-model i)
                  (form->hiccup (eval-parsed-expr i false post-validator))
                  i))
        parsed-form))
     (do (eval form-nmspc)
         (clojure.walk/postwalk
        (fn [i] (if (m/validate parsed-expr-model i)
                  (form->hiccup (eval-parsed-expr i false post-validator))
                  i))
        parsed-form))))
  ([parsed-form form-nmspc] (eval-with-errors parsed-form form-nmspc (fn [e] {:result e})))
  ([parsed-form] (eval-with-errors parsed-form (symbol (str *ns*)))))

(defn eval-in-ns
  [expr nmspc]
  (binding [*ns* (create-ns (symbol nmspc))]
    (do
      (refer-clojure)
      (eval expr))))

(defn eval-expr-ns
  "Evaluates the given EDN string expr in the given ns with the given deps."
  [expr nmspc deps]
  (let [yield? (.startsWith expr "=")
        exp (if yield?
              (subs expr 1)
              expr)
        current-ns *ns*]
    (binding [*ns* (create-ns (symbol nmspc))]
      (do
        (refer-clojure)
        (if deps (apply require deps))
        (if yield?
          (eval (r/read-string exp))
          (do (eval (r/read-string exp)) nil))))))

(defn include-source
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
  [:map {:description "File metadata used by fabricate"}
   [:filename {:description "The title of the file, absent any prefixes"} :string]
   [:file-extension {:description "The extension of the output format of the file"} :string]
   [:fabricate/suffix {:description "The suffix following the output file extension"} :string]
   [:created {:optional true :description "When the file was created"} :string]
   [:modified {:optional true :description "When the file was modified"}]])

(comment
  (def working-dir
    (io/file (System/getProperty "user.dir")))



  (def example-file
    (io/file "./pages/index.html.fab"))

  (def dir-local-path
    (.relativize (.toPath (io/file ".")) (.toPath example-file)))

  (.relativize (.toPath (io/file ".")) (.toPath example-file))

  (.relativize (.toPath (io/file ".")) (.toPath (io/file "pages/index.html.fab")))

  )

(defn get-file-metadata [file-path]
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

(defn ->dir-local-path [file]
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
