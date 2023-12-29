(ns site.fabricate.prototype.api-test
  (:require [clojure.test :as t]
            [site.fabricate.prototype.api :as api]
            [babashka.fs :as fs]))

(t/deftest entries
  (let [tmp-dir (fs/create-temp-dir {:prefix "fabricate-test-"})
        entry-data (api/plan {:site.fabricate.page/publish-dir tmp-dir})]
    (t/testing "planning"
      (t/is (every? #(contains? % :site.fabricate.source/location) entry-data)))
    (t/testing "assembly"
      (doseq [e entry-data]
        (let [assembled (api/assemble e)]
          (t/is (some? (:site.fabricate.document/data assembled))))))
    (t/testing "production"
      (doseq [e entry-data]
        (let [assembled (api/assemble e)
              {:keys [site.fabricate.page/output], :as produced} (api/produce!
                                                                  assembled)]
          (t/is (some? (:site.fabricate.page/title produced))
                (format "Page produced from %s should have a title"
                        (str (:site.fabricate.source/location assembled))))
          (t/is (instance? java.io.File output))
          (t/is (fs/exists? output)))))))

(comment
  (fs/create-temp-dir {:prefix "something-"})
  (def assembled-posts-test
    (mapv api/assemble (api/plan {:input-dir "pages", :patterns api/patterns})))
  (first assembled-posts-test))
