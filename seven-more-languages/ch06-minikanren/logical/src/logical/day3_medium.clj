(ns logical.day3-medium)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.day3-book)
(use 'logical.utils)

;; we need to push stroy events to the end to learn about murderers.
;; to do so, we can get all resources available after generated events,
;; try applying more events to consume these resources (and get new resources)
;; until nothing can be further consumed.

;; make an dictionary of actions from story-elements
(defn mk-action-dict
  [story-elements]
  (defn combine
    [ad [k v]]
    (assoc ad k (conj (get ad k []) v)))
  (reduce combine {} story-elements))

(defn day3-medium
  []
  (p "day 3 - do medium")
  (p (mk-action-dict story-elements))
  )
