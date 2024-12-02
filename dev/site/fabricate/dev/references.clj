(ns site.fabricate.dev.references
  "Namespace for resolving references"
  (:require [clojure.string :as str]))



(def namespace-paths
  "Map containing paths to all the HTML reference files for each Fabricate namespace"
  (atom (->> (all-ns)
             (filter #(let [ns-str (str (ns-name %))]
                        (and (str/starts-with? ns-str "site.fabricate")
                             (not (re-find #"adorn" ns-str))
                             (not (re-find #"docs" ns-str))
                             (not (re-find #"notes" ns-str))
                             (not (re-find #"dev" ns-str)))))
             (map (fn [nmspc]
                    (let [ns-sym (ns-name nmspc)]
                      [ns-sym (str "/reference/namespaces/" ns-sym ".html")])))
             (into {}))))

(defn identifier
  "Generate a valid HTML ID from the given term"
  [term]
  (str/replace term #"\s+" "_"))

(def glossary-path (atom "/reference/glossary.html"))
(defn anchor-link [term] (str @glossary-path "#" (identifier term)))



(defn- specify-dispatch
  [term]
  (if-let [term-meta (meta term)]
    (or (:type term-meta) (type term))
    (type term)))

;; separation of concerns: specify produces the data necessary
;; to generate an actual anchor link element

(defmulti specify
  "Specify the term, first using metadata (if available) and then using the type."
  specify-dispatch)

(defmethod specify java.lang.String
  [term]
  {:term       term
   :link       (anchor-link term)
   :type       :term
   :identifier (identifier term)})

(defmethod specify clojure.lang.Namespace
  [nmspc]
  (let [ns-sym  (ns-name nmspc)
        ns-meta (meta nmspc)]
    {:type       :namespace
     :term       (str nmspc)
     :namespace  nmspc
     :doc        (:doc ns-meta)
     :identifier (str nmspc)
     :link       (:doc/url ns-meta
                           (str "/reference/namespaces/" ns-sym ".html"))}))

(defmethod specify clojure.lang.Symbol
  [term]
  (let [sym-meta (meta term)
        sym-ns   (namespace term)]
    {:type       :symbol
     :symbol     term
     :doc        (:doc sym-meta)
     :identifier (str term)
     :link       (:doc/url sym-meta
                           (if sym-ns
                             (str "/reference/namespaces/" sym-ns
                                  ".html#" (name term))
                             (str @glossary-path term)))}))

(defmethod specify clojure.lang.Var
  [term]
  (let [var-meta (meta term)]
    {:type       :var
     :symbol     term
     :doc        (:doc var-meta)
     :identifier (str term)
     :link       (:doc/url var-meta
                           (str "/reference/namespaces/" (namespace (symbol
                                                                     term))
                                ".html#" (name (symbol term))))}))

(defmethod specify clojure.lang.Keyword
  [term]
  {:type       :keyword
   :keyword    term
   :identifier (str term)
   :link       (if-let [kw-ns (namespace term)]
                 (str "/reference/namespaces/" kw-ns
                      ".html#" (keyword (name term)))
                 (str @glossary-path term))})

(comment
  (specify #'site.fabricate.api/plan!)
  (specify-dispatch #'site.fabricate.api/plan!)
  (name (symbol #'str))
  (str (symbol #'str))
  (str 'str)
  @namespace-paths
  (str (ns-name *ns*))
  (str *ns*))

(defn reference
  "Generate a Hiccup reference element for the given term."
  [{:keys [term link] :as d}]
  [:a {:href link :class "reference" :data-term term :title term} term])

(comment
  (reference 'site.fabricate.api)
  (reference "Source Location")
  (anchor-link "Source Location"))
