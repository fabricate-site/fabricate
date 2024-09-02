(ns site.fabricate.prototype.performance-test
  (:require [clojure.test :as t]
            [criterium.core :as bench]
            [clj-async-profiler.core :as profiler]
            [taoensso.tufte :as tufte]
            [clojure.test.check :as check]
            [clojure.test.check.properties :as prop]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.hiccup :as hiccup]
            [site.fabricate.prototype.html :as html]
            [site.fabricate.prototype.hiccup-test :as hiccup-test]))

(t/deftest ^:performance paragraph-detection
  (t/testing "profiling call graph of paragraph detection"
    #_(check/quick-check 100
                         (prop/for-all [html-elem hiccup-test/html-newline-gen]
                                       (profiler/start)
                                       (hiccup/parse-paragraphs html-elem)
                                       (profiler/stop)))
    (profiler/profile (dotimes [_ 500]
                        (hiccup/parse-paragraphs
                         [:div {:class "row"} "orphan text"
                          [:em "with emphasis added"] "and\n\nlinebreak"]))))
  (t/testing "benchmarking execution time"
    (bench/quick-bench (hiccup/parse-paragraphs
                        [:div {:class "row"} "orphan text"
                         [:em "with emphasis added"] "and\n\nlinebreak"]))))

(t/deftest ^:performance paragraph-detection-revised
  (with-redefs [html/permitted-contents html/tag-contents]
    (t/testing "profiling call graph of paragraph detection"
      #_(check/quick-check 100
                           (prop/for-all [html-elem page-test/html-newline-gen]
                                         (profiler/start)
                                         (hiccup/parse-paragraphs html-elem)
                                         (profiler/stop)))
      (profiler/profile (dotimes [_ 500]
                          (hiccup/parse-paragraphs
                           [:div {:class "row"} "orphan text"
                            [:em "with emphasis added"] "and\n\nlinebreak"]))))
    (t/testing "benchmarking execution time"
      (bench/quick-bench (hiccup/parse-paragraphs
                          [:div {:class "row"} "orphan text"
                           [:em "with emphasis added"] "and\n\nlinebreak"])))))

(t/deftest ^:performance page-parsing
  (t/testing "performance of page parsing"
    (let [f (slurp "pages/index.html.fab")]
      (profiler/profile (dotimes [_ 500] (read/parse f)))
      (bench/quick-bench (read/parse f)))))

;; idea: interceptible evaluation functions, so you could do things like
;; profile the evaluation of pages by changing the FSM definition

(comment
  (profiler/serve-files 9001))
