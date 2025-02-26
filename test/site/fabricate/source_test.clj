(ns site.fabricate.source-test
  (:require [clojure.test :as t]
            [site.fabricate.api :as api]
            [site.fabricate.source :as source]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]))

(t/deftest default-multimethods
  (t/testing "clojure"
    (let [entries (api/collect "**/*.clj"
                               {:site.fabricate.source/location "."})]
      (doseq [e entries]
        (t/testing (:site.fabricate.source/location e)
          (let [built-entry (api/build e {})]
            (when-not (m/validate api/entry-schema built-entry)
              (println (me/humanize (m/explain api/entry-schema built-entry))))
            (t/is (m/validate api/entry-schema built-entry)
                  "Arbitrary clojure sources should build without errors")))))))
