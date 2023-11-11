(ns site.fabricate.prototype.api-test
  (:require [clojure.test :as t]
            [site.fabricate.prototype.api :as api]
            [babashka.fs :as fs]))

(t/deftest entries
  (let [entry-data (api/plan {:patterns api/patterns, :input-dir "pages"})]
    (t/testing "planning"
      (t/is (every? #(instance? java.io.File
                                (:site.fabricate.page/input-file %))
                    entry-data)))
    (t/testing "assembly"
      (doseq [e entry-data]
        (let [assembled (api/assemble e)]
          (t/is (some? (:site.fabricate.page/data assembled))))))
    (t/testing "production"
      (let [tmp-dir (fs/create-temp-dir {:prefix "fabricate-test-"})]
        (doseq [e entry-data]
          (let [assembled (assoc-in (api/assemble e)
                                    [:site.fabricate.page/output
                                     :site.fabricate.page/output-dir]
                                    tmp-dir)
                {:keys [site.fabricate.page/output-file], :as produced}
                (api/produce! assembled)]
            (t/is (instance? java.io.File output-file))
            (t/is (fs/exists? output-file))))))))

(comment
  (fs/create-temp-dir {:prefix "something-"})
  (def assembled-posts-test
    (mapv api/assemble (api/plan {:input-dir "pages", :patterns api/patterns})))
  (first assembled-posts-test))
