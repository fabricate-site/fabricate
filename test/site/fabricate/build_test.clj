(ns site.fabricate.build-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [site.fabricate.api :as api]
            [site.fabricate.dev.build :as build]
            [site.fabricate.prototype.html :as html]
            [malli.core :as m]
            [babashka.fs :as fs]
            [clojure.java.io :as io]))

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

(defn register-collect-methods!
  ([pattern-formats]
   (doseq [[pattern fmt] pattern-formats]
     (defmethod api/collect pattern
       [pattern {:keys [site.fabricate.source/dir] :as opts}]
       (t/testing (str pattern ":")
         (let [pattern (str (fs/file "**" (fs/file-name pattern)))
               results (mapv (fn [p]
                               ;; using namespace-specific formats ensures
                               ;; that the tests never clash with other
                               ;; default multimethods
                               {:site.fabricate.source/format fmt
                                :site.fabricate.document/format ::hiccup
                                :site.fabricate.page/format ::hiccup-html
                                ::api/source pattern
                                :site.fabricate.source/original-location
                                (:site.fabricate.source/original-location opts)
                                :site.fabricate.source/location (fs/file p)})
                             (fs/glob dir pattern))]
           (t/is (every? fs/exists?
                         (map :site.fabricate.source/location results))
                 (str "every collected entry should exist"))
           results)))))
  ([] (register-collect-methods! pattern-formats)))

(def entry? (m/validator api/Entry))

(defn valid-fabricate-hiccup? [hiccup-data])

(defn build-fabricate
  "Build and test the entry from a Fabricate template"
  [entry]
  {:pre [(t/is (entry? entry))] :post [(t/is (and (entry? %)))]}
  entry)

#_(defmethod api/build)
#_(defmethod api/build)

(defn valid-output-hiccup? [hiccup-data] (html/element? hiccup-data))

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
                            (#'site.fabricate.api/assemble [])
                            (#'site.fabricate.api/construct! []))
                       :done)))))
      (t/testing "validity of generated page contents"
                 ;; TODO: validate the Hiccup passed to Chassis during
                 ;; `api/produce!` using the HTML namespace
      ))))

(t/deftest sites
  ;; only define the multimethods at runtime
  (register-collect-methods!)
  (run! test-site (into [manual-site-config] additional-sites))
  ;; clean up + unmap test-specific multimethod impls
  (run! (partial remove-method api/collect) (keys pattern-formats)))
