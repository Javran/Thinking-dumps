(ns logical.day2-medium)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)
(use 'logical.day1-book)
(use 'logical.day1-medium)

(defn unsungo [xs]
  (matche [xs]
    ([[]])
    ([ [hd . tl]]
     ;; hd should be a scientist at the first place
     (scientisto hd)
     (conda
      [(fresh [y]
         (turingo hd y))
       ;; make it impossible to unify in this branch
       ;; at this point there's no way to go back
       ;; so we've somehow managed to express negation
       (== 1 2)]
      [(unsungo tl)]))))

(defn day2-medium
  []
  (p "day 2 - do medium")
  (defn test-list [xs]
    (with-db facts (run* [q] (unsungo xs))))
  ;; element is not scienist -- should fail
  (p (test-list [1 2 3]))
  ;; none of them has won Turing award -- should succeed
  (p (test-list [:alan-turing :grace-hopper :alonzo-church]))
  ;; should fail because :leslie-lamport won Turing award
  (p (test-list [:alan-turing :grace-hopper :leslie-lamport :alonzo-church])))
