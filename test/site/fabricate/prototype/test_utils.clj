(ns site.fabricate.prototype.test-utils
  (:require
   [malli.core :as m]
   [malli.instrument :as mi]
   [clojure.test :as t]))

(defn with-instrumentation [f]
  (mi/collect!)
  #_(mi/instrument!)
  (f)
  #_(mi/unstrument!))
