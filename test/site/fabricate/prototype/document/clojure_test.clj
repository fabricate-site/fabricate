(ns site.fabricate.prototype.document.clojure-test
  (:require [clojure.test :as t]
            [babashka.fs :as fs]
            [malli.core :as m]
            [site.fabricate.prototype.document.clojure :as clj]))

(def valid-form-map? (m/validator clj/form-map-schema))

(t/deftest parsing
  (t/testing "parsing file into sequence of forms with rewrite-clj"
    (doseq [src-file (fs/glob "src" "**.clj")]
      (let [forms (clj/file->forms (fs/file src-file))]
        (t/is (every? valid-form-map? forms))))))


(comment
  print-dup)
