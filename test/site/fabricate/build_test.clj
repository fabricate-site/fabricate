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
            [site.fabricate.dev.html :as html-check]
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
            [malli.util :as mu]))




(def manual-site-config
  {:site.fabricate.api/options
   {:html/debug? false
    :site.fabricate.page/publish-dir (fs/create-temp-dir)
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

(defn collect-entry
  [pattern fmt opts p]
  ;; using namespace-specific formats ensures
  ;; that the tests never clash with other
  ;; default multimethods
  {:site.fabricate.source/file (fs/file (fs/canonicalize (fs/absolutize p)))
   :site.fabricate.source/format fmt
   :site.fabricate.document/format ::hiccup
   :site.fabricate.page/format :chassis/hiccup
   ::api/source pattern
   :site.fabricate.source/original-location
   (:site.fabricate.source/original-location opts)
   :site.fabricate.source/directory (fs/parent (fs/canonicalize (fs/absolutize
                                                                 p)))
   :site.fabricate.source/location (fs/file (fs/canonicalize (fs/absolutize
                                                              p)))})

(defn register-collect-methods!
  ([pattern-formats]
   (doseq [[pattern fmt] pattern-formats]
     (defmethod api/collect pattern
       [pattern {:keys [site.fabricate.source/dir] :as opts}]
       (t/testing (str pattern ":")
         (let [pattern (str (fs/file "**" (fs/file-name pattern)))
               results (mapv
                        (partial collect-entry pattern fmt opts)
                        #_(fn collect-entry [p]
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

(defn build-fabricate
  "Build and test the entry from a Fabricate template"
  [{:keys [site.fabricate.source/location site.fabricate.source/file] :as entry}
   opts]
  {:pre  [(t/is (valid-schema? props/CollectedEntry entry))]
   :post [(t/is (valid-schema? props/FabricateHiccupEntry %))]}
  (let [hiccup-article (fab/entry->hiccup-article entry opts)
        page-metadata  (meta hiccup-article)]
    (assoc entry
           :site.fabricate.document/data  hiccup-article
           :site.fabricate.document/title (:title page-metadata))))

(defn build-clojure
  "Build and test the entry from a Clojure source"
  [{:keys [site.fabricate.source/location] :as entry} opts]
  {:pre  [(t/is (valid-schema? props/CollectedEntry entry))]
   :post [(t/is (valid-schema? props/FabricateHiccupEntry %))]}
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
        (merge page-metadata))))

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
    [entry opts]
    (build-clojure entry opts)))

(def assemble-tasks [])

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

(defn render-kindly
  "Render the kindly form into an output format"
  {:malli/schema props/=>RenderForm}
  [v]
  {:post [(t/is (= :ok (props/classify-render-output v %)))]}
  (api/render-form v))

(defn process-kinds
  [form page-format]
  (try (walk/postwalk (fn process? [v] (if (kindly-map? v) (render-kindly v) v))
                      form)
       (catch Exception e
         (do (println "encountered an error processing form")
             #_(pprint/pprint form)))))

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
            validation-result (html-check/validate-html-string output-html)]
        ;; uncomment this when the schema supports lists/seqs of elements!
        #_(t/is (valid-schema? html/element data)
                "api/produce should produce valid Hiccup elements")
        (binding [pprint/*print-miser-width* 60
                  pprint/*print-right-margin* 75
                  *print-length* 10
                  *print-level* 7]
          ;; make this configurable, eventually
          #_(pprint/pprint
             (mapv #(select-keys % ["type" "subType" "extract" "message"])
                   (get validation-result "messages")))
          (t/is (valid-schema? html-check/ValidHTMLOutput
                               validation-result)))))))

(defn unregister-multimethods!
  "clean up + unmap test-specific multimethod impls"
  [site]
  (run! (partial remove-method api/collect) (keys pattern-formats))
  (run! (partial remove-method api/build)
        [[::fabricate ::hiccup] [::clojure ::hiccup]
         [:site.fabricate.read/v0 :hiccup]
         [:site.fabricate.markdown/v0 :markdown] [:clojure/v0 :hiccup]
         [:clojure/deps :clojure/edn]])
  (run! (partial remove-method api/produce!)
        [[::hiccup :chassis/hiccup] [:hiccup :html] [:markdown :markdown]
         [:clojure/edn :clojure/edn]])
  (run! (partial remove-method api/display-form)
        [[:kind/hiccup :chassis/hiccup] [nil :hiccup/html]
         [:hiccup :hiccup/html]])
  site)


(t/use-fixtures :once test-utils/with-instrumentation unregister-multimethods!)


(defn test-site
  [{:keys [site.fabricate.dev.build/setup-tasks site.fabricate.api/options]
    :or   {setup-tasks []}
    :as   site-config}]
  ;; only define the multimethods at runtime
  (register-collect-methods!)
  (register-build-methods!)
  (register-produce-methods!)
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
  (if (test-utils/cli-test?) (match-config/enable-abbreviation!))
  (run! test-site (into [manual-site-config] additional-sites)))
