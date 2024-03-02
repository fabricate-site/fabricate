(ns site.fabricate.prototype.order "Namespace for sorting elements of a map")

;; Kahn's algorithm

;; L ← Empty list that will contain the sorted elements
;; S ← Set of all nodes with no incoming edge

;; while S is not empty do
;; remove a node n from S
;; add n to L
;; for each node m with an edge e from n to m do
;; remove edge e from the graph
;; if m has no other incoming edges then
;; insert m into S

;; if graph has edges then
;; return error   (graph has at least one cycle)
;; else
;; return L   (a topologically sorted order)

;; edge is defined by the map as
;; [k,v] = k -> v ("task k depends on task v")
;; special case of tasks with no requirements: [k, nil]
;; these are by definition "nodes with no incoming edge"


(defn topological-sort
  [m]
  ;; cycle detection: does L contain k already?
  ;; return nil if cycle is detected
  (loop [S (set (for [[k v] m :when (nil? v)] k)) L #{}]))
