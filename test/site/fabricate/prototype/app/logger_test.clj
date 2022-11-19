(ns site.fabricate.prototype.app.logger-test
  (:require [com.brunobonacci.mulog :as u]
            [clojure.edn :as edn]
            [site.fabricate.prototype.write :refer [default-site-settings]]
            [clojure.test :as t]
            [clojure.java.io :as io])
  (:import [java.io File]))

(t/deftest logger-event-capture

  (t/testing "ability of logger to capture and surface relevant events"

    (let [test-file (File/createTempFile "test-logs-" ".edn")
          file-pub
          (u/start-publisher!
           {:type :simple-file
            :filename (str test-file)
            :transform (get-in default-site-settings
                               [:site.fabricate.app.logger/config
                                :transform])})]

      (u/with-context {:log/level 800}

        (u/log ::test-event)

        (u/log ::test-low-level-event :log/level 200)

        (u/trace ::test-trace {:capture (fn [r] {:output/value r})}
                 (do (println "test event") :done)))

      (let [log-data (edn/read-string
                      (str "[" (slurp test-file) "]"))]

        (t/is (not-empty log-data))
        (t/is (= 2 (count log-data)))
        (t/is (not-any? #(= ::test-low-level-event (:mulog/event-name %))
                        log-data)))

      (file-pub)
      (io/delete-file test-file))))

(comment

  (with-out-str (u/log ::test-event :some/data 3)))
