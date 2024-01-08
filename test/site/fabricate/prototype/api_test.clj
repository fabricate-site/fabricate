(ns site.fabricate.prototype.api-test
  (:require [clojure.test :as t]
            [site.fabricate.prototype.api :as api]
            ;;  refer the dev ns to ensure multimethods have impls
            [site.fabricate.dev.build]
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


(t/deftest entries
  (let [tmp-dir (fs/create-temp-dir {:prefix "fabricate-test-"})
        entry-data (api/plan! [(fn [_] (fs/create-dir (fs/path tmp-dir "css")))]
                              {:site.fabricate.page/publish-dir tmp-dir})]
    (t/testing "planning"
      (t/is (every? (m/validator api/entry-schema) entry-data)))
    (t/testing "assembly"
      (doseq [e entry-data]
        (let [assembled (api/assemble e)]
          (t/is (some? (:site.fabricate.document/data assembled))))))
    (t/testing "combine"
      (let [assembled (mapv #(api/assemble %) entry-data)
            combined (api/combine [#(mapv doc->page
                                          (:site.fabricate.api/entries %))]
                                  {:site.fabricate.api/entries assembled})]
        (t/is (contains? (first combined) :site.fabricate.page/data))))
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
  (api/build! {})
  (fs/create-temp-dir {:prefix "something-"})
  (def assembled-posts-test
    (mapv api/assemble (api/plan {:input-dir "pages", :patterns api/patterns})))
  (first assembled-posts-test))
