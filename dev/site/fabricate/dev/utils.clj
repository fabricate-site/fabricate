(ns site.fabricate.dev.utils
  "Dev-time utilities"
  (:require
   [clojure.test]
   [site.fabricate.api]
   [site.fabricate.prototype.clojure]
   [site.fabricate.prototype.eval]
   [site.fabricate.prototype.hiccup]
   [site.fabricate.prototype.html]
   [site.fabricate.prototype.kindly]
   [site.fabricate.prototype.page.hiccup]
   [site.fabricate.prototype.read]
   [site.fabricate.prototype.schema]
   [site.fabricate.prototype.template]))

(defn fabricate-vars
  "Get a seq of every public var in Fabricate"
  ([opts]
   (->> (all-ns)
        (filter (fn [nmspc]
                  (let [ns-str (str nmspc)]
                    (and (re-find #"^site\.fabricate" ns-str)
                         (not (re-find #"^site\.fabricate\.adorn" ns-str))
                         (not (re-find #"^site\.fabricate\.dev" ns-str))
                         (not (re-find #"^site\.fabricate.*test" ns-str))
                         (not (re-find #"^site\.fabricate.example" ns-str))
                         (not (re-find #"^site\.fabricate.*ephemeral" ns-str))
                         (not (re-find #"^site\.fabricate.*docs" ns-str))
                         (not (re-find #"^site\.fabricate.*time" ns-str))))))
        (mapcat #(map second (ns-publics %)))
        (sort-by #(str (namespace (symbol %))))
        (into [])))
  ([] (fabricate-vars {})))

(defn list-fabricate-vars
  ([opts]
   (let [var-list (fabricate-vars)]
     (println "Fabricate public vars:" (count var-list) "total")
     (println "")
     (doseq [v var-list]
       (println v)
       (println (str "  type: "
                     (if (clojure.test/function? (var-get v))
                       "function"
                       (type (var-get v)))))
       (println (str "  " (:doc (meta v))))
       (println ""))))
  ([] (list-fabricate-vars {})))


(comment
  (list-fabricate-vars))
