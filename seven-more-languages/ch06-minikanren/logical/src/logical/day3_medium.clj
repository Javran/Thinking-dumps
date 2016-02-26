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

;; we first get the current state by applying ("replaying") events
;; on the initial state.
;; this could have been done without "replaying", but that requires modifying
;; the original function to keep the final state.
;; here we keep the original lib intact.
(defn apply-action
  [state [k v]]
  (let [old-res-ind (.indexOf state k)]
    (assert (not= old-res-ind -1)
            (str "resource not available: " k))
    (conj (filter #(not= k %) state) v)))

(defn apply-actions
  [state actions]
  (reduce apply-action state actions))

(defn available-events
  [state action-dict]
  (distinct
   (mapcat
    (fn [res]
      (map (fn [t] [res t])
           (get action-dict res [])))
    state)))

(defn day3-medium
  []
  (p "day 3 - do medium")
  (let [events
        (first
         (filter #(> (count %) 5)
                 (with-db story-db
                   (run* [q]
                     (storyo [:guilty-peacock :dead-yvette] q)))))
        adict
        (mk-action-dict story-elements)
        current-state
        (apply-actions start-state events)]
    (p (available-events current-state adict))))
