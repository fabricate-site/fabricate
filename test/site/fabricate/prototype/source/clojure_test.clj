(ns site.fabricate.prototype.source.clojure-test
  (:require [clojure.test :as t]
            [babashka.fs :as fs]
            [malli.core :as m]
            [malli.error :as me]
            [site.fabricate.prototype.source.clojure :as clj]))



(t/deftest extract-functions
  (t/testing "clojure comments"
    (t/is
     (=
      "in the assemble step, should the Clojure code be treated as a \"block\"?\n"
      (#'clj/extract-comment-text
       ";; in the assemble step, should the Clojure code be treated as a \"block\"?\n")))
    (t/is (= "a comment " (#'clj/extract-comment-text ";; a comment ")))))

(t/deftest parsing
  (t/testing "comment extraction")
  (let [valid-form-map? (m/validator clj/form-map-schema)
        form-explainer  (m/explainer clj/form-map-schema)]
    (t/testing "parsing file into sequence of forms with rewrite-clj"
      (doseq [src-file (fs/glob "." "**.clj")]
        (t/testing (str "\n" src-file)
          (let [forms      (clj/file->forms (fs/file src-file))
                all-valid? (every? valid-form-map? forms)]
            (when-not all-valid?
              (doseq [invalid-form (filter #(not (valid-form-map? %)) forms)]
                (println (me/humanize (form-explainer invalid-form)))))
            (t/is all-valid? "Each parsed form should be valid.")))))))


(comment
  clojure.java.io/do-copy
  print-dup)
