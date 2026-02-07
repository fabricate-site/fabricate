(ns site.fabricate.build-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [site.fabricate.api :as api]
            [site.fabricate.dev.build :as build]
            [babashka.fs :as fs]
            [clojure.java.io :as io]))

(def manual-site-config
  {:site.fabricate.page/publish-dir       (fs/create-temp-dir)
   :site.fabricate.build-test/setup-tasks (drop-last
                                           3
                                           site.fabricate.dev.build/setup-tasks)
   :site.fabricate.source/source-dir
   ;; eventually the manual should provide pages to the library
   ;; using resources on the classpath so it can be: (io/resource ...)
   ;; so these tests run in CI
   "../fabricate-manual/docs"})

(defn path->site-config
  [site-dir]
  {:site.fabricate/site-name         (str (fs/parent site-dir))
   :site.fabricate.source/source-dir site-dir})

(def additional-sites
  (if-let [test-sites (System/getProperty "site.fabricate.build-test.dirs")]
    (mapv path->site-config (str/split test-sites #","))
    []))

(defn test-site
  [{:keys [site.fabricate.source/source-dir
           site.fabricate.dev.build/setup-tasks]
    :or   {setup-tasks []}
    :as   site-config}]
  (when (fs/exists? source-dir)
    (t/testing (str source-dir ":")
      (t/testing (str "ability to build without errors")
        (t/is (= :done
                 (do (->> {:site.fabricate.api/options site-config}
                          (#'site.fabricate.api/plan! setup-tasks)
                          (#'site.fabricate.api/assemble [])
                          (#'site.fabricate.api/construct! []))
                     :done)))))
    (t/testing "validity of generated page contents"
               ;; TODO: validate the Hiccup passed to Chassis during
               ;; `api/produce!` using the HTML namespace
    )))

(t/deftest sites (run! test-site (into [manual-site-config] additional-sites)))
