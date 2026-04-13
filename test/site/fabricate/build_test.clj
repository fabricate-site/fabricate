(ns site.fabricate.build-test
  (:require [clojure.test :as t]
            [clojure.pprint :as pprint]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as match]
            [matcher-combinators.config :as match-config]
            [clojure.string :as str]
            [clojure.set :as set]
            [site.fabricate.api :as api]
            [site.fabricate.dev.build :as build]
            [site.fabricate.prototype.html :as html]
            [site.fabricate.prototype.eval :as eval]
            [site.fabricate.prototype.kindly :as kindly]
            [site.fabricate.prototype.document.clojure :as clj]
            [site.fabricate.prototype.page.hiccup :as hiccup]
            [site.fabricate.prototype.page :as page]
            [site.fabricate.prototype.hiccup :as prototype.hiccup]
            [site.fabricate.prototype.document.fabricate :as fab]
            [site.fabricate.prototype.properties :as props]
            [site.fabricate.prototype.test-utils :as test-utils]
            [dev.onionpancakes.chassis.core :as chassis]
            [malli.core :as m]
            [malli.dev.pretty :as mp]
            [malli.error :as me]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.data.json :as json]
            [malli.util :as mu])
  (:import [nu.validator.validation SimpleDocumentValidator]
           [nu.validator.client EmbeddedValidator]
           [java.io ByteArrayInputStream]))

(t/use-fixtures :once test-utils/with-instrumentation)

(def manual-site-config
  {:site.fabricate.api/options
   {:site.fabricate.page/publish-dir (fs/create-temp-dir)
    :site.fabricate.build-test/setup-tasks
    (drop-last 3 site.fabricate.dev.build/setup-tasks)
    ;; eventually the manual should provide pages to the library
    ;; using resources on the classpath so it can be: (io/resource ...)
    ;; so these tests run in CI
    :site.fabricate.source/dir "../fabricate-manual/docs"
    :site.fabricate.source/original-location "../fabricate-manual/docs"}})

;; Fabricate's multimethods are flexible enough to allow them to be used in
;; its own test suite to ensure properties hold about actual sites being built.

(defonce test-dir (fs/create-temp-dir {:prefix "fabricate-test-"}))

(defn path->site-config
  [site-dir]
  {:site.fabricate.api/options
   {:site.fabricate/site-name  (str (fs/parent site-dir))
    :site.fabricate.source/original-location site-dir
    :site.fabricate.source/dir site-dir}})

(defn copy-to-test-dir!
  "Copies the site's source dir to the test location."
  [{:keys [site.fabricate.api/options] :as site-config}]
  ;; https://stackoverflow.com/questions/24834116/how-can-i-get-clojure-pre-post-to-report-their-failing-value
  {:post [(t/is (fs/exists? (:site.fabricate.source/dir %))
                "Test directory should be copied successfully")]}
  (let [dir (:site.fabricate.source/dir options)
        new-location (fs/file test-dir (fs/file-name dir))]
    (fs/copy-tree dir new-location {:replace-existing true})
    (assoc site-config :site.fabricate.source/dir new-location)))

(def setup-tasks [copy-to-test-dir!])

(defn drop-relative-to-parent
  [f]
  (apply fs/file (drop 1 (fs/components (fs/file f)))))

(def additional-sites
  (if-let [test-sites (System/getProperty "site.fabricate.build-test.dirs")]
    (mapv path->site-config (str/split test-sites #","))
    []))

(def pattern-formats
  {(str test-dir "/**/*.fab") ::fabricate (str test-dir "/**/*.clj") ::clojure})

(defn build-test-collect [] nil)

(defn register-collect-methods!
  ([pattern-formats]
   (doseq [[pattern fmt] pattern-formats]
     (defmethod api/collect pattern
       [pattern {:keys [site.fabricate.source/dir] :as opts}]
       (t/testing (str pattern ":")
         (let [pattern (str (fs/file "**" (fs/file-name pattern)))
               results (mapv (fn collect-entry [p]
                               ;; using namespace-specific formats ensures
                               ;; that the tests never clash with other
                               ;; default multimethods
                               {:site.fabricate.source/file
                                (fs/file (fs/canonicalize (fs/absolutize p)))
                                :site.fabricate.source/format fmt
                                :site.fabricate.document/format ::hiccup
                                :site.fabricate.page/format :chassis/hiccup
                                ::api/source pattern
                                :site.fabricate.source/original-location
                                (:site.fabricate.source/original-location opts)
                                :site.fabricate.source/directory
                                (fs/parent (fs/canonicalize (fs/absolutize p)))
                                :site.fabricate.source/location
                                (fs/file (fs/canonicalize (fs/absolutize p)))})
                             (fs/glob dir pattern))]
           (t/is (valid-schema? (m/schema [:* props/CollectedEntry]) results)
                 (str "every collected entry should match expected schema"))
           results)))))
  ([] (register-collect-methods! pattern-formats)))


(def entry? (m/validator api/Entry))


(defn debug-entry-error
  [{:keys [path in value schema] :as error}]
  (println "Did not match schema")
  (pprint/pprint schema)
  (if (test-utils/cli-test?)
    (binding [*print-length* 5 *print-level* 1] (pprint/pprint value))
    (binding [*print-length* 20 *print-level* 4] (pprint/pprint value))))


(defn valid-output-hiccup? [hiccup-data] (html/element? hiccup-data))

(defn build-fabricate
  "Build and test the entry from a Fabricate template"
  [entry]
  {:pre [(t/is (entry? entry))] :post [(t/is (and (entry? %)))]}
  entry)

(defn build-clojure
  "Build and test the entry from a Clojure source"
  [entry]
  {:pre [(t/is (entry? entry))] :post [(t/is (and (entry? %)))]}
  entry)

(defn register-build-methods!
  []
  (defmethod api/build [::fabricate ::hiccup]
    [{:keys [site.fabricate.source/location site.fabricate.source/file]
      :as   entry} opts]
    (let [hiccup-article (fab/entry->hiccup-article entry opts)
          page-metadata  (meta hiccup-article)]
      (assoc entry
             :site.fabricate.document/data  hiccup-article
             :site.fabricate.document/title (:title page-metadata))))
  (defmethod api/build [::clojure ::hiccup]
    [{:keys [site.fabricate.source/location] :as entry} opts]
    (let [data (-> location
                   clj/read-forms
                   clj/eval-forms
                   clj/forms->hiccup)
          page-metadata (-> data
                            (get-in [1 :data-clojure-namespace])
                            (find-ns)
                            meta)]
      (-> entry
          (assoc :site.fabricate.document/data data)
          (merge page-metadata)))))

(defn check-hiccup-entries
  [{:keys [site.fabricate.api/entries] :as site}]
  (binding [*print-length* (if (test-utils/cli-test?) 5 10)
            *print-level*  (if (test-utils/cli-test?) 1 5)]
    (doseq [{:keys [site.fabricate.source/location] :as e} entries]
      (when (#{::hiccup} (e :site.fabricate.document/format))
        (t/testing (str "\nentry: " location)
          (t/is (valid-schema? props/FabricateHiccupEntry e))))))
  site)

(def assemble-tasks [check-hiccup-entries])

(def key-lookup
  {:kind        :data-kind
   :kindly/kind :data-kind
   :kindly/hide-code :data-kindly-hide-code})
(defn with-keys
  [map kmap]
  (-> map
      (select-keys (keys kmap))
      (set/rename-keys kmap)))

(defn process-hiccup-form
  [{:keys [kind value] :as form}]
  #_(when (nil? value)
      (throw (ex-info "Hiccup element should not be nil" {:kindly-map form})))
  (when value
    (let [data-attrs (with-keys form key-lookup)]
      (chassis/apply-normalized (fn add-data-attrs [tag attrs contents]
                                  (into [tag (merge attrs data-attrs)]
                                        contents))
                                value))))

(def kindly-map? (m/validator eval/Evaluated-Form))

;; TODO: reimplement the post assertions as part of a malli schema
(defn process-kinds
  [form page-format]
  (try
    (walk/postwalk
     (fn [v]
       (if (kindly-map? v)
         (let [{:keys [kind value kindly/hide-code kindly/hide-value] :as v}
               (assoc v :site.fabricate.page/format page-format)
               output (api/render-form v)
               unexpected-nil-output?
               (and (nil? output) (some? value) (not (:kindly/hide-value v)))
               unexpected-output (and hide-code hide-value (some? output))]
           (t/is
            (not unexpected-output)
            "Values should not be returned when :kindly/hide-code and :kindly/hide-value are both true")
           (when unexpected-nil-output? (pprint/pprint v))
           (t/is
            (not unexpected-nil-output?)
            (str
             "Non-nil values should not be returned as nil after rendering kind "
             kind
             " to output format " page-format))
           (t/is (not (kindly-map? output))
                 "All kindly maps should be transformed")
           output)
         v))
     form)
    (catch Exception e
      (do (println "encountered an error processing form")
          #_(pprint/pprint form)))))

(def html-validator
  (doto (EmbeddedValidator.)
    (.setOutputFormat nu.validator.client.EmbeddedValidator$OutputFormat/JSON)))

(defn validate-html-string
  [html-string]
  (let [html-input-stream (ByteArrayInputStream. (.getBytes html-string))]
    (try (let [validator-output (.validate html-validator html-input-stream)]
           (if (empty? validator-output) {} (json/read-str validator-output)))
         (catch Exception e (Throwable->map e)))))

(comment
  (validate-html-string (chassis/html [chassis/doctype-html5
                                       [:head [:title "test"]] [:body]])))

(defn register-produce-methods!
  []
  (defmethod api/display-form [:fabricate/error :hiccup/html]
    [{:keys [value] :as form}]
    (site.fabricate.prototype.read/error->hiccup (assoc form :error value)))
  (defmethod api/produce! [::hiccup :chassis/hiccup]
    [{:keys [site.fabricate.document/title] :as entry} opts]
    (t/testing (str "\n" title)
      (let [{:keys [site.fabricate.document/data] :as processed-entry}
            (update entry
                    :site.fabricate.document/data
                    #(process-kinds % :hiccup/html))
            output-hiccup     [chassis/doctype-html5
                               (hiccup/entry->hiccup-head processed-entry)
                               (hiccup/entry->hiccup-body processed-entry)]
            output-html       (try (chassis/html output-hiccup)
                                   (catch Exception e
                                     "<<<HTML RENDERING ERROR>>>"))
            validation-result (validate-html-string output-html)]
        #_(t/is (match? {:site.fabricate.document/title string?}
                        processed-entry)
                "Each entry should have a title")
        ;; uncomment this when the schema supports lists/seqs of elements!
        #_(t/is (valid-schema? html/element data)
                "api/produce should produce valid Hiccup elements")
        (binding [pprint/*print-miser-width*  60
                  pprint/*print-right-margin* 75]
          (pprint/pprint
           (mapv #(select-keys % ["type" "subType" "extract" "message"])
                 (get validation-result "messages"))))
        (t/is
         (empty?
          (into
           []
           (remove
            #(or
              (#{"info" "warning"} (get % "type"))
              (and
               (= "error" (get % "type"))
               (or
                ;; <dt> within <div> elements within <dl> elements have
                ;; been valid HTML since 2017 (HTML5.2)
                (re-find
                 #"Element “dt” not allowed as child of element “div” in this context."
                 (get % "message"))
                (re-find #"skipping \d heading levels" (get % "message"))
                ;; known bug: current version of
                ;; validator detects CSS layer
                ;; directive as invalid
                (re-find #"CSS.*layer" (get % "message"))
                ;; no support for CSS subgrid in 2026?
                (re-find #"CSS.*subgrid" (get % "message"))
                (re-find #"CSS.*" (get % "message"))))))
           (get validation-result "messages"))))))))



(defn unregister-multimethods!
  "clean up + unmap test-specific multimethod impls"
  [site]
  (run! (partial remove-method api/collect) (keys pattern-formats))
  (run! (partial remove-method api/build)
        [[::fabricate ::hiccup] [::clojure ::hiccup]])
  (run! (partial remove-method api/produce!) [[::hiccup :chassis/hiccup]])
  (run! (partial remove-method api/display-form)
        [[:kind/hiccup :chassis/hiccup]])
  site)

(defn test-site
  [{:keys [site.fabricate.dev.build/setup-tasks site.fabricate.api/options]
    :or   {setup-tasks []}
    :as   site-config}]
  (let [{:keys [site.fabricate.source/dir]} options]
    (when (fs/exists? dir)
      (t/testing (str dir ":")
        (t/testing (str "ability to build without errors")
          (t/is (= :done
                   (do (->> site-config
                            (#'site.fabricate.api/plan! setup-tasks)
                            (#'site.fabricate.api/assemble assemble-tasks)
                            (#'site.fabricate.api/construct!
                             [unregister-multimethods!]))
                       :done))))))))

(t/deftest sites
  ;; only define the multimethods at runtime
  (register-collect-methods!)
  (register-build-methods!)
  (register-produce-methods!)
  (if (test-utils/cli-test?) (match-config/enable-abbreviation!))
  (run! test-site (into [manual-site-config] additional-sites)))
