(ns site.fabricate.prototype.source.clojure-test
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
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

(t/deftest node-functions
  (t/testing "metadata normalization"
    (t/is (= {:kindly/hide-code true}
             (#'clj/meta-node->metadata-map
              (parser/parse-string "^:kindly/hide-code [1 2 3]")))
          "keyword metadata should be normalized into a map")
    (t/is (= {:type 'Double}
             (#'clj/meta-node->metadata-map (parser/parse-string "^Double a")))
          "keyword metadata should be normalized into a map")
    (t/is
     (thrown? clojure.lang.ExceptionInfo
              (#'clj/meta-node->metadata-map (node/coerce [1 2 3 4])))
     "Nil/non-metadata nodes should throw an error when conversion is attempted")))

(t/deftest parsing
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

(defn correct-example?
  "Is the example correct?"
  [{clj-form    :clojure/form
    clj-result  :clojure/result
    clj-error   :clojure/error
    clj-comment :clojure/comment
    :as         evaluated-form}]
  (or (and (some? clj-form) (nil? clj-error))
      (and (some? clj-error) (= "testing" (get-in clj-error [:context])))
      (string? clj-comment)))

(t/deftest evaluation
  (t/testing "parsing and evaluation of the example file"
    (let [evaluated-results (-> "test-resources/site/fabricate/example.clj"
                                clj/file->forms
                                clj/eval-forms)]
      (t/is (map? evaluated-results))
      (doseq [{clj-form :clojure/form :as result-form} (:clojure/forms
                                                        evaluated-results)]
        (t/testing result-form
          (t/is
           ;; can't use some? here because a form and result can be nil!
           (or (and (contains? result-form :clojure/form)
                    (contains? result-form :clojure/result))
               (contains? result-form :clojure/comment)
               (contains? result-form :clojure/newlines)
               (contains? result-form :clojure/whitespace)
               (= {:severity :trivial :context "testing"}
                  (get-in result-form [:clojure/error :data])))))))))


(comment
  (io/resource "site/fabricate/example.clj")
  clojure.java.io/do-copy
  print-dup)
