(ns site.fabricate.prototype.check
  "Namespace to check output HTML against validator.nu HTML tests."
  (:require [clojure.java.io :as io]
            [babashka.fs :as fs])
  (:import [nu.validator.validation SimpleDocumentValidator]
           [nu.validator.messages TextMessageEmitter MessageEmitterAdapter]
           [nu.validator.source SourceCode]
           [nu.validator.xml SystemErrErrorHandler]))

(def validator
  "Default HTML Validator object."
  (let [v         (SimpleDocumentValidator.)
        sys-error (SystemErrErrorHandler.)]
    (doto v
      (.setUpMainSchema "http://s.validator.nu/html5-rdfalite.rnc" sys-error)
      (.setUpValidatorAndParsers sys-error true false))))

(defn html
  "Test the given directories and files"
  [{:keys [dirs files]}]
  (let [all-files (->> dirs
                       (mapcat #(fs/glob % "**.html"))
                       (map fs/file)
                       (into (map fs/file files)))]
    (doseq [f all-files] (.checkHtmlFile validator f true))))
