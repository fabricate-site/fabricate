(ns site.fabricate.dev.elements)

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
