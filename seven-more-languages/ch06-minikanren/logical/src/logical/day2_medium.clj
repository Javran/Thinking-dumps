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
     (unsungo tl)
     )))

(defn day2-medium
  []
  (p "day 2 - do medium")
  (p
   (with-db facts (run* [q] (unsungo [:alan-turing :grace-hopper]))))
  )
