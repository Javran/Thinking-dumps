(ns logical.day3-medium)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.day3-book)
(use 'logical.utils)

;; We need to push stroy events to the end to learn about murderers.
;; to do so, we can get all resources available after generated events,
;; try applying more events to consume these resources (and get new resources)
;; until nothing can be further consumed.
;; A potential problem of this might be the inconsistency of the stroy,
;; since we are not using the logic framework anymore, we have to ensure
;; the consistency ourselves. However, in this case this is not a problem
;; as long as we guarantee that one resource can only be consumed once.

;; make an dictionary of actions from story-elements
(defn mk-action-dict
  [story-elements]
  (defn combine
    [ad [k v]]
    (assoc ad k (conj (get ad k []) v)))
  (reduce combine {} story-elements))

(defn apply-action
  [state [k v]]
  (let [old-res-ind (.indexOf state k)]
    (assert (not= old-res-ind -1)
            (str "resource not available: " k))
    (conj (filter #(not= k %) state) v)))

(defn day3-medium
  []
  (p "day 3 - do medium")
  (p (apply-action [:maybe-motorist :dead-telegram-girl]
                   [:maybe-motorist :guilty-peacock]))
  )
