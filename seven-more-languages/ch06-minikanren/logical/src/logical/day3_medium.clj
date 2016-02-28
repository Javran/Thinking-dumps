(ns logical.day3-medium)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.day3-book)
(use 'logical.day3-easy)
(use 'logical.utils)

;; We need to push stroy events to the end to learn about murderers.
;; to do so, we can get all resources available after generated events,
;; try applying more events to consume these resources (and get new resources)
;; until nothing can be further consumed.
;; A potential problem of this might be the inconsistency of the stroy,
;; since we are not using the logic framework anymore, we have to ensure
;; the consistency ourselves. However, in this case this is not a problem
;; as long as we guarantee that one resource can only be consumed once.
;; (I prefer "actions" over "events" in code below, but
;; there is no difference)

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

(defn available-actions
  [state action-dict]
  (distinct
   (mapcat
    (fn [res]
      (map #(conj [res] %)
           (get action-dict res [])))
    state)))

(defn push-story
  [state action-dict]
  (let [next-actions
        (available-actions state action-dict)]
    (if (empty? next-actions)
      ;; we can perform no more actions
      [state []]
      (let [next-action (first (shuffle next-actions))
            next-state (apply-action state next-action)
            [result-state actions] (push-story next-state action-dict)]
        [result-state (into [next-action] actions)]))))

;; In theory the way we make the story complete is also capable of generating strories
;; on its own: given a set of resources, we are just finding paths of actions
;; to take us to certain set of resources available.
;; This remains be a question of what logic-based approach gives us.
;; In my opinion, it is the expresssiveness: we can easily describe
;; what is a desired state we want to reach, without
;; enforcing extra restrictions (e.g. giving a full set of end-resources
;; while only part of it is intended). And the search task is performed
;; by the system. In addition, logic-based system is more capable of eliminating
;; invalid states (some cases might be that certain resources cannot coexist,
;; this problem does not exist for this example though).

;; this approach has one drawback:
;; all consumable resources have to be comsumed in the end.
;; this make some optional resources (those prefixed with :maybe-)
;; no longer optional.

;; complete the story by pushing it forward until
;; no more resource can be consumed
(defn complete-story
  [start-state actions story-elements]
  (let [adict
        (mk-action-dict story-elements)
        current-state
        (apply-actions start-state actions)
        [final-state extra-actions]
        (push-story current-state adict)]
    (concat actions extra-actions)))

(defn day3-medium
  []
  (p "day 3 - do medium")
  (p "exercise 1")
  (let [events
        (first
         (filter
          ;; we will make the story complete anyway,
          ;; so it won't make much trouble if the generated story is a bit short
          #(> (count %) 5)
          (with-db story-db
            (run* [q]
              (storyo [:guilty-peacock :dead-yvette] q)))))]
    (print-story
     (complete-story
      start-state events story-elements)))

  (p "exercise 1, extra")
  ;; we try to push stories generated in day3_easy.clj
  ;; to the end so we might have more than 2 murderers
  ;; see comments in that file for detail
  (print-story
   (complete-story
    day3e-exercise2-start-state
    (first
     (filter
      ;; feel "3" is faster
      #(> (count %) 3)
      (day3e-exercise2-story-stream)))
    story-elements))

  (p "exercise 2")
  ;; this idea needs some refinement to work:
  ;; for example, :dead-mr-boddy could be consumed and
  ;; produce one of the following:
  ;; :guilty-plum, :guilty-scarlet :guilty-peacock
  ;; but we cannot produce them at the same time
  ;; (that might mean one victim is killed by more than one murderer)
  ;; I think a better way would be to allow events to consume one resource
  ;; but produce a list of them, by doing so, we can consume :motorist
  ;; and produce [:dead-motorist :policeman] in addition to the existing
  ;; possibilities of producing exactly one of :policeman or :dead-motorist
  ;;)
