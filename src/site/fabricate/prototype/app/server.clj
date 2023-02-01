(ns site.fabricate.prototype.app.server
  (:require
   [org.httpkit.server :as server]
   [reitit.core :as r]
   [reitit.ring :as ring]
   #_[compojure.core :as cmp :refer [routes GET]]
   #_[compojure.route :as route]
   [ripley.html :as h]
   [ripley.live.context :as context]
   [ripley.live.source :as source]
   [ripley.live.protocols :as p]
   [ripley.js :as js]
   [clojure.string :as str]
   [hiccup.core :as hiccup]
   [clojure.java.io :as io]
   [babashka.fs :as fs])
  (:import [java.io File]))


;; target 1: replicate the nasus view

;; target 2: surface metadata about pages (e.g. render time)

;; target 3: make it fashion

(comment
  (-main)

  )



(defonce server (atom nil))

;; Some file utilities

(defn- matching-files [path name-filter]
  (mapcat
   (fn [file]
     (if (.isDirectory file)
       (matching-files file name-filter)
       (when (and
              (.endsWith (.getName file) ".html")
              (str/includes? (.getName file) name-filter))
         [file])))
   (.listFiles path)))

(defn- paths-leading-to-matching-files [path matching-files]
  (let [->absolute #(.getAbsolutePath %)
        top-path (->absolute path)]
    (if (empty? matching-files)
      #{}
      (into #{top-path}
            (comp
             (mapcat (fn [matching-file]
                       (take-while #(not= top-path (->absolute %))
                                   (drop 1 (iterate #(.getParentFile %) matching-file)))))
             (map ->absolute))
            matching-files))))

;; UI components

(declare folder)

(defn files [{:keys [name-filter] :as ctx} path]
  (h/html
   [:div {:style "padding-left: 1rem;"}
    [:div
     [::h/for [file (.listFiles path)
               :let [name (.getName file)
                     url (.toString (fs/relativize path file))]]
      [::h/if (.isDirectory file)
       (folder ctx file)
       [:div {:style [::h/live (source/computed
                                #(or (str/blank? %)
                                     (str/includes? name %)) name-filter)
                      #(if %
                         "padding-left: 1.5rem;"
                         "display: none;")]}
        [:a {:href url} name]]]]]]))

(defn folder [{:keys [expanded toggle-expanded!] :as ctx} path]
  (let [name (-> path .getCanonicalFile .getName)
        id (.getAbsolutePath path)
        expanded? (source/computed #(contains? % id) expanded)]
    (h/html
     [::h/live expanded?
      (fn [expanded?]
        (h/html
         [:div
          [:div {:style "display: flex;"}
           [:button {:on-click (partial toggle-expanded! id)}
            [::h/if expanded? "-" "+"]]
           [:span name]]
          (files ctx path)]))])))

(defn search! [set-name-filter! set-expanded! path new-name-filter]
  ;; Expand all paths and parents that contain matching files
  (let [paths (if (str/blank? new-name-filter)
                #{}
                (paths-leading-to-matching-files
                 path (matching-files path new-name-filter)))]
    (set-name-filter! new-name-filter)
    (set-expanded! paths)))

(defn filetree-app [path]
  (let [[expanded set-expanded!] (source/use-state #{})
        [name-filter set-name-filter!] (source/use-state "")]
    (h/html
     [:div
      [:h2 "Fabricate"]
      [:h3 "Rendered pages"]
      [:label "Page filter"]
      [:input#name-filter
       {:type "text"
        :on-input (js/js-debounced 500
                                   #(search! set-name-filter! set-expanded! path %)
                                   (js/input-value :name-filter))}]
      (folder {:name-filter name-filter
               :expanded expanded
               :toggle-expanded! (fn [path]
                                   (let [cur (p/current-value expanded)]
                                     (set-expanded!
                                      ((if (cur path)
                                         disj conj) cur path))))}
              path)])))

(defn filetree-page [path]
  (h/html
   [:html
    [:head
     [:title "Fabricate: Pages"]]
    [:body
     (h/live-client-script "/__ripley-live")
     (filetree-app path)]]))


(defn filetree-routes [path]
  (ring/ring-handler
   (ring/router
    [["/ping" (constantly {:status 200 :body "pong"})]
     ["/preview" {:get (constantly
                        (h/render-response (partial filetree-page path)))}]])
   (ring/routes
    (ring/create-file-handler
     {:path "/" :root "docs"
      :index-files ["fabricate.html"]})
    (context/connection-handler "/__ripley_live")
    (ring/create-default-handler
     {:not-found (constantly {:status 404
                              :body "This is not a space"})}))))


(defn- restart
  ([] (restart 3000 "docs"))
  ([port path]
   (swap!
    server
    (fn [old-server]
      (when old-server
        (old-server))
      (println "Starting server")
      (server/run-server (filetree-routes (File. path)) {:port port})))))

(defn -main [& _args]
  (restart))

(comment
  (-main)


  )
