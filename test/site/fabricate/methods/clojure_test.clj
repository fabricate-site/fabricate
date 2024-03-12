(ns site.fabricate.methods.clojure-test
  (:require [clojure.test :as t]
            [babashka.fs :as fs]
            [site.fabricate.methods.clojure :as clj]))

(t/deftest parsing
  (t/testing "parsing file into sequence of forms with rewrite-clj"
    (doseq [src-file (fs/glob "src" "**.clj")]
      (t/is (coll? (clj/file->forms (fs/file src-file))))
      )))
