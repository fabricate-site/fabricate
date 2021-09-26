(ns site.fabricate.prototype.check
  (:require [clojure.java.io :as io])
  (:import [nu.validator.validation SimpleDocumentValidator]
           [nu.validator.messages TextMessageEmitter MessageEmitterAdapter]
           [nu.validator.source SourceCode]
           [nu.validator.xml SystemErrErrorHandler]
           ))

(def sax-error (SystemErrErrorHandler. ))

(def validator
  (let [v (SimpleDocumentValidator. )]
    (doto v
      (.setUpMainSchema "http://s.validator.nu/html5-rdfalite.rnc" sax-error)
      (.setUpValidatorAndParsers
       sax-error
       true
       false))))

(defn html [{:keys [dirs files]}]
  (let [all-files
        (->> dirs
             (map #(file-seq (io/file %)))
             flatten
             (concat (map io/file files))
             (filter #(.endsWith (.toString %) ".html")))]
    (doseq [f all-files]
      (.checkHtmlFile validator f true))))

(comment

  (.exists
   (io/file "/home/andrew/repos_main/fabricate/docs/index.html"))

  (.checkHtmlFile validator
   (io/file "/home/andrew/repos_main/fabricate/docs/index.html") false)

  (.checkHttpURL validator
                 "https://fabricate-site.github.io/fabricate"
                 "default"
                 sax-error
                 )
  (html {:dirs ["docs/"]})

  )
