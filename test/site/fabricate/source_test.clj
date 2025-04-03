(ns site.fabricate.source-test
  (:require [clojure.test :as t]
            [site.fabricate.api :as api]
            [site.fabricate.source :as source]
            [babashka.fs :as fs]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]))



(t/deftest default-multimethods
  (t/testing "collecting clojure sources"
    (let [entries (api/collect "**/*.clj"
                               {:site.fabricate.source/location (fs/file ".")})]
      (doseq [e entries]
        (t/testing (:site.fabricate.source/location e)
          (t/is (m/validate api/entry-schema e)))))))
