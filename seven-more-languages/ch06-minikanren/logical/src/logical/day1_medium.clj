(ns logical.day1-medium)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)
(use 'logical.day1-book)

(defn scientisto [p]
  (conde
   [(mano p)]
   [(womano p)]))


(defn day1-medium
  []
  (p "day 1 - do medium")
  (p "exercise 1")
  (p
   (with-db facts
     (run* [p]
       (scientisto p))))
  (p "exercise 2")
  ;; find all scientists who've won Turing Awards
  (p
   (with-db facts
    (run* [p]
      (fresh [y]
        (scientisto p)
        (turingo p y))))))
