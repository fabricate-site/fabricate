(ns site.fabricate.prototype.api-test
  (:require [clojure.test :as t]
            [site.fabricate.prototype.api :as api]
            [site.fabricate.dev.build] ; refer the dev ns to ensure
            ; multimethods have impls
            [site.fabricate.prototype.test-utils :refer [with-instrumentation]]
            [malli.core :as m]
            [babashka.fs :as fs]))

(defn doc->page
  [{:keys [site.fabricate.document/title site.fabricate.document/data
           site.fabricate.document/id],
    :as entry}]
  (assoc entry
         :site.fabricate.page/data data
         :site.fabricate.page/title title
         :site.fabricate.page/id id))

(t/use-fixtures :once with-instrumentation)


(t/deftest api-ops
  (let [tmp-dir (fs/create-temp-dir {:prefix "fabricate-test-"})
        {:keys [site.fabricate.api/entries], :as plan-data}
        (api/plan! [(fn [s] (fs/create-dir (fs/path tmp-dir "css")) s)]
                   {:site.fabricate.api/options
                    {:site.fabricate.page/publish-dir tmp-dir}})]
    (t/testing "planning"
      (t/is (every? (m/validator api/entry-schema) entries)))
    (t/testing "building"
      (doseq [e entries]
        (let [built (try (api/build e {})
                         (catch Exception exc
                           (do (tap> (dissoc (Throwable->map exc) :trace))
                               (tap> e)
                               nil)))]
          (t/is (some? (:site.fabricate.document/data built))))))
    (t/testing "assembly"
      (let [assembled (api/assemble [#(mapv doc->page
                                            (:site.fabricate.api/entries %))]
                                    {:site.fabricate.api/entries entries})]
        (t/is (contains? (first assembled) :site.fabricate.page/data))))
    (t/testing "production"
      (doseq [e entries]
        (let [built (api/build e {})
              _ (do (t/is (some? (api/produce-dispatch built {}))))
              {:keys [site.fabricate.page/output], :as produced}
              (api/produce! built {})]
          (t/is (some? (:site.fabricate.page/title produced))
                (format "Page produced from %s should have a title"
                        (str (:site.fabricate.source/location built))))
          (t/is (instance? java.io.File output))
          (t/is (fs/exists? output)))))))

(defn unmap-multimethods
  "Utility function to ease reloading of redefined multimethods."
  []
  (run! #(ns-unmap (find-ns 'site.fabricate.prototype.api) %)
        '[collect build produce!]))

(comment
  (unmap-multimethods)
  (.getMethodTable api/produce!)
  (api/build! {})
  (fs/create-temp-dir {:prefix "something-"})
  (first built-posts-test))
