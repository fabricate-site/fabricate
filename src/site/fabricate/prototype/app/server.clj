(ns site.fabricate.prototype.app.server
  (:require [org.httpkit.server :as server]))

;; target 1: replicate the nasus view

;; target 2: surface metadata about pages (e.g. render time)

;; target 3: make it fashion


(defn app [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<h1>FABRICATE</h1>"})

(defonce state (atom nil))

(defn stop-server []
  (when-not (nil? @state)
    (@state :timeout 100)
    (reset! state nil)))

(defn -main [& args]
  (reset! state (server/run-server #'app {:port 8000})))

(comment
  (-main)

  )
