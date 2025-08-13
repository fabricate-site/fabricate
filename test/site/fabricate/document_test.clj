(ns site.fabricate.document-test
  "Testing default build methods for documents"
  (:require [site.fabricate.api :as api]
            [site.fabricate.source :as source]
            [site.fabricate.document :as document]
            [site.fabricate.prototype.document.clojure :as clj]
            [site.fabricate.prototype.document.fabricate :as fabricate]
            [babashka.fs :as fs]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]
            [clojure.test :as t]))


(def example-entries
  [{::source/location (fs/file "test-resources/site/fabricate/example.clj")
    ::api/source      "example"
    ::source/format   :clojure/file
    ::document/format :hiccup/article}
   {::source/location (fs/file "test-resources/site/fabricate/example.clj")
    ::source/format   :clojure/file
    ::api/source      "example"
    ::document/format :kind/fragment}
   #_{::source/location ""
      ::source/format   ::fabricate/v0
      ::document/format :hiccup/article}])


(def post-build-entry (m/schema (mu/required-keys api/Entry [::document/data])))

(def skip-files
  {(fs/file "test/site/fabricate/prototype/html_test.clj")
   "Obscure rewrite-clj parser error for ::html/p in this file"})

(t/deftest default-multimethods
  (doseq [{:keys         [::source/location]
           source-format ::source/format
           document-format ::document/format
           :as           entry}
          #_example-entries
          (api/collect "**/*.clj"
                       {:site.fabricate.source/location (fs/file ".")})]
    (when-not (skip-files location)
      (t/testing (str "example entry at " location
                      " " [source-format document-format])
        (let [built-entry (api/build entry {})
              valid-entry (m/validate post-build-entry built-entry)]
          (when-not valid-entry
            (println (me/humanize (m/explain post-build-entry built-entry))))
          (t/is valid-entry
                (str "entry at "
                     location
                     " should produce a valid entry after building"))))
      (when-not (= "example.clj" (str (fs/file-name location)))
        (load-file (str location))))))
