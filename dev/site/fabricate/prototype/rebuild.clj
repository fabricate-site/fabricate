(ns site.fabricate.prototype.rebuild
  (:require [site.fabricate.api :as api])
  (:import [java.util.concurrent LinkedBlockingQueue]))

;; Until I know this works, I think it may be better to keep in a separate
;; namespace.

;; Design considerations

;; 1. single, separate thread running a loop that shuts down gracefully
;; 2. sequential execution only

;; non-goal: dynamic redefinition of configuration. if I want to get crazy with
;; my own website code that's fine, but stability is the key for a public API.
;; if settings change, restart. even if Clojure had Erlang-like supervision
;; trees in the stdlib, that's a lot of complexity to manage for Fabricate's
;; users.


;; one way to skip over the unchanged entries would be to temporarily `dissoc`
;; the necessary keys for `api/build` + `api/produce!` from the unchanged
;; entries, then re-add them to the site yielded by `assemble` and `construct!`

;; this feels inelegant, but it may be the least-bad option, as it puts the
;; complexity into app state management rather than the API, which is arguably
;; where the complexity belongs.

;; except that won't work, because `api/assemble`'s tasks expect to operate on
;; everything, not just the delta of updated entries.

;; the other way would be to just define new versions of `api/assemble` and
;; `api/construct` that only execute `api/build` and `api/produce!`


(defn match-file-sources [new-entries] (let [sources nil]))

(defn- merge-entries
  [existing-entries new-entries]
  (mapv (fn [{existing-location :site.fabricate.source/location :as entry}]
          (let [new-entry (first (filter (fn [{new-location
                                               :site.fabricate.source/location}]
                                           (= existing-location new-location))
                                         new-entries))]
            (or new-entry entry)))
        existing-entries))

(defn- reassemble
  [tasks
   {:keys [site.fabricate.api/entries site.fabricate.api/options
           rebuild/new-entries]
    :as   site}]
  (let [sort-fn  (get options :site.fabricate.api/entry-sort-fn identity)
        new-docs (mapv (fn [e] (api/build e options)) (sort-fn new-entries))])
  (reduce (fn [site task] (task site))
          (-> site
              (update :site.fabricate.api/entries merge-entries new-entries)
              (assoc :rebuild/new-entries new-entries))))

(defn- reconstruct!
  [tasks
   {:keys [site.fabricate.api/entries site.fabricate.api/options
           rebuild/new-entries]
    :as   site}]
  (doseq [e new-entries] (api/produce! e options))
  (reduce (fn [site task] (task site)) (dissoc site :rebuild/new-entries)))

;; The fact that I was able to override the default behavior of the functions
;; merely by including an additional key suggests that this could in fact be
;; supported in the core API without a separate namespace.

;; that probably isn't a good move yet. this idea needs to bake a bit longer.

;; one possible way of distinguishing: an *optional*
;; `:site.fabricate.entry/updated?` flag.

;; the problem is that having an `updated?` key in one entry implicitly
;; affects the behavior of the remaining entries by implying they're
;; *not* updated.

;; The rebuild/merge entries functions can attempt to distinguish by
;; adding a `updated?` `false` k/v to the entry map of any entry already in
;; the site but not returned by the rebuild fn. this way, only entries with
;; `updated?` `false` get skipped, potentially enabling the main API to
;; handle both cases

(comment
  (def ^:private app (agent {:state :waiting :process nil}))
  (def site "Current state of the site." (atom {}))
  (defn rebuild!
    "`api/plan!`, `api/assemble`, and `api/construct!` the site, then begin watching sources for changes.

  When changes are detected for a source, the following events happen in order:
  1. Changed source entries are unified with unchanged source entries - if two entries
     with the same `site.fabricate.source/location` exist in the site, the newer is used.
  2. The site and `assemble-tasks` are passed to `api/assemble`. Unchanged posts are skipped by `build`.
  3. The site and `construct-tasks` are passed to `api/construct`. Unchanged posts are skipped by `produce`.
  4. Await new changes.


  If `rebuild!` is already running, calling `rebuild!` again will shut down the watch functions."
    [{:keys [site.fabricate.api/entries site.fabricate.api/options]
      :as   initial-site}
     {:keys [plan-tasks assemble-tasks construct-tasks shutdown-tasks]
      :as   tasks}]
    (if (= :running (:state @app))
      ;; shutdown may need to go into its own function
      (do (send-off app
                    (fn [s]
                      (let [final-site (reduce (fn [site task] (site task))
                                               @site
                                               shutdown-tasks)]
                        (reset! site final-site))
                      (-> s
                          (update :process #(do (future-cancel %) nil))
                          (assoc :state :stopped))))
          :stopped)
      (let [built-site (->> initial-site
                            (api/plan! plan-tasks)
                            (api/assemble assemble-tasks)
                            (api/construct! construct-tasks))]
        (reset! site built-site)
        ;; this doesn't feel like it's correctly implemented; needs more
        ;; design work
        (let [proc (future (send app assoc :state :running)
                           (await app)
                           (loop [ag app]
                             (when (= :running (:state @ag))
                               (do (println "running") (Thread/sleep 5000))
                               (recur app))))]
          (send app assoc :process proc))
        :running))))



;; extremely basic, but effective example of the "queue of thunks" idea

(def action-queue (LinkedBlockingQueue.))

(def status (atom {:queue :waiting}))

(defn drain-queue
  [^LinkedBlockingQueue q]
  (let [drain-list (java.util.ArrayList. [])]
    (.drainTo q drain-list)
    (vec drain-list)))

(defn update-state
  [{:keys [queue] :as s}]
  (let [new-state
        (condp = queue
          :running    (let [action (.poll action-queue)]
                        (if action
                          (do (println "executing")
                              (try (do (action) :running)
                                   (catch Exception e
                                     (do (println "error")
                                         (println (Throwable->map e))
                                         :waiting))))
                          :running))
          :waiting    :waiting
          :stopped    (let [remaining-actions (drain-queue action-queue)]
                        ;; should this also short-circuit on the first
                        ;; failure or attempt every remaining action?
                        (loop [[action & others] remaining-actions]
                          (let [result (try
                                         (do (println "executing") (action) :ok)
                                         (catch Exception e
                                           (do (println "error")
                                               (println (Throwable->map e))
                                               :failed)))]
                            (cond (= result :ok)     (recur others)
                                  (= result :failed) :failed)))
                        :terminated)
          :terminated (do (.clear action-queue) :terminated))]
    (assoc s :queue new-state)))


(def ^:dynamic *process* (future (while true (swap! status update-state))))
