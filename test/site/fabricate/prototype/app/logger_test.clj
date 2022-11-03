(ns site.fabricate.prototype.app.logger-test
  (:require [com.brunobonacci.mulog :as u]
            [site.fabricate.prototype.write :refer [default-site-settings]]
            [clojure.test :as t]
            [clojure.java.io])
  (:import [java.io File]))


(t/deftest logger-event-capture

  (t/testing "ability of logger to capture and surface relevant events"

    (let [test-file (File/createTempFile "test-logs" ".edn")
          file-pub
          (u/start-publisher!
           {:type :simple-file
            :filename (str test-file)
            :transform (get-in default-site-settings
                               [:site.fabricate.app.logger/config
                                :transform])})]

      (u/log ::test-event)

      (println (slurp test-file))
      (file-pub))

    )
  )


(comment



  (with-out-str (u/log ::test-event :some/data 3))

  )
