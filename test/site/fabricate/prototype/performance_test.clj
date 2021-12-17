(ns site.fabricate.prototype.performance-test
  (:require  [clojure.test :as t]
             [criterium.core :as bench]
             [clj-async-profiler.core :as profiler]
             [taoensso.tufte :as tufte]
             [site.fabricate.prototype.read :as read]
             [site.fabricate.prototype.page :as page]))

(t/deftest ^:performance paragraph-detection
  (t/testing "performance of paragraph detection"
    (profiler/profile
     (dotimes [_ 500]
       (page/parse-paragraphs
        [:div
         {:class "row"}
         "orphan text"
         [:em "with emphasis added"]
         "and\n\nlinebreak"])))
    (bench/quick-bench
     (page/parse-paragraphs
        [:div
         {:class "row"}
         "orphan text"
         [:em "with emphasis added"]
         "and\n\nlinebreak"]))))

(t/deftest ^:performance page-parsing
  (t/testing "performance of page parsing"
    (let [f (slurp "pages/fabricate.html.fab")]
      (profiler/profile
       (bench/bench (read/parse f))))))

;; idea: interceptible evaluation functions, so you could do things like
;; profile the evaluation of pages by changing the FSM definition

(comment
  (profiler/serve-files 9001)

  )
