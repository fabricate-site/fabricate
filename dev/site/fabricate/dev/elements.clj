(ns site.fabricate.dev.elements
  (:require [dev.onionpancakes.chassis.core :as c]))

(defn footer
  ([top-id]
   [:footer {:class "main-footer"}
    [:div {:class "footer-home"}
     [:a {:class "decor-internal" :href "/"} "Home"]]
    [:div {:class "footer-api"}
     [:a
      {:class "decor-internal"
       :href  "/reference/namespaces/site.fabricate.api.html"} "API"]]
    [:div {:class "footer-namespaces"}
     [:a {:class "decor-internal" :href "/reference/namespaces.html"}
      "Namespaces"]]
    [:div {:class "footer-top"}
     [:a {:class "decor-internal" :href top-id} "Top"]]])
  ([] (footer "#top")))

(def logo-img
  [:img {:src "/media/fabricate-logo-v1.svg" :class "fabricate-logo"}])

(def header-big
  [:header {:class "header-big card"}
   [:img {:src "/media/fabricate-logo-v1.svg" :class "fabricate-header-logo"}]
   [:h1 {:class "header-primary"} "Fabricate"]
   [:h3 {:class "header-secondary"} "Form by art and labor"]])

(defn escape-css-string
  "Escape the string so it can be used as a valid CSS identifier"
  [str])

(defn anchor
  "Generate a unique anchor from the given symbol."
  [sym]
  (let [resolved (resolve sym)]))

(comment
  (c/escape-attribute-value (str (symbol (resolve 'anchor)))))

(defn function-doc
  [fn-var]
  (let [id (anchor fn-var)]
    ;; TODO: come up with canonical, or at least stable, way of converting
    ;; Clojure symbols and vars to and from properly escaped CSS
    ;; identifiers. the rest kind of flows from there, but each function
    ;; needs a UUID-like thing so that a URL can be generated from its
    ;; fully-qualified name
    [:div {:class "fn-doc" :id id :data-clojure-var (str fn-var)}]))
