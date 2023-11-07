(ns site.fabricate.prototype.api-test
  (:require [clojure.test :as t]
            [site.fabricate.prototype.api :as api]
            [babashka.fs :as fs]))

(t/deftest entries
  (let [entry-data (api/plan {:patterns api/patterns, :input-dir "pages"})]
    (t/testing "planning"
      (t/is (every? #(instance? java.io.File (:site.fabricate/input-file %))
                    entry-data)))
    (t/testing "assembly"
      (doseq [e entry-data]
        (let [assembled (api/assemble e)]
          (t/is (some? (:page/data assembled))))))))
