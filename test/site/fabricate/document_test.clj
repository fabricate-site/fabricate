(ns site.fabricate.document-test
  "Testing default build methods for documents"
  (:require [site.fabricate.api :as api]
            [site.fabricate.source :as source]
            [site.fabricate.document :as document]
            [site.fabricate.prototype.document.clojure :as clj]
            [site.fabricate.prototype.document.fabricate :as fabricate]
            [site.fabricate.prototype.properties :as props]
            [site.fabricate.prototype.test-utils :as tu]
            [babashka.fs :as fs]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]
            [clojure.test :as t]))


(def example-entries
  [{:site.fabricate.source/location
    (fs/file "test-resources/site/fabricate/example.clj")
    :site.fabricate.api/source "example"
    :site.fabricate.source/format :clojure/file
    :site.fabricate.document/format :hiccup/article}
   {:site.fabricate.source/location
    (fs/file "test-resources/site/fabricate/example.clj")
    :site.fabricate.source/format :clojure/file
    :site.fabricate.api/source "example"
    :site.fabricate.document/format :kind/fragment}
   #_{:source/location ""
      :source/format   :fabricate/v0
      :document/format :hiccup/article}])


(def post-build-entry
  (m/schema (mu/required-keys api/Entry [:site.fabricate.document/data])))

(def skip-files {})

(t/deftest default-multimethods
  (doseq [{:keys         [:site.fabricate.source/location]
           source-format :site.fabricate.source/format
           document-format :site.fabricate.document/format
           :as           entry}
          #_example-entries
          (api/collect "**/*.clj"
                       {:site.fabricate.source/location (fs/file ".")})]
    (when-not (skip-files location)
      (t/testing (str "example entry at " location
                      " " [source-format document-format])
        (let [built-entry (try (api/build entry {})
                               (catch Exception e
                                 (throw (-> e
                                            Throwable->map
                                            (assoc :context entry)
                                            (#(ex-info "Error building entry"
                                                       %))))))]
          (tu/check-schema (malli.util/dissoc props/BuiltEntry
                            :site.fabricate.document/title)
                           built-entry)))
      (when-not (= "example.clj" (str (fs/file-name location)))
        (load-file (str location))))))
