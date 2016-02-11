(ns logical.day1-easy)

(use 'clojure.core.logic)
(use 'logical.utils)

(defn day1-easy
  []
  (p "exercise 1")
  ;; whatever element present in both list are shown up as results
  (p (run* [q] (membero q [1 2 3]) (membero q [4 3 2])))
  ;; (2 3)
  ;; and duplicated elements are all shown
  (p (run* [q] (membero q [1 2 3]) (membero q [4 3 2 1 1])))
  ;; (1 1 2 3)
  ;; returns no result if nothing in common
  (run* [q] (membero q [1 2 3]) (membero q [4 5 6]))
  ;; ()

  (p "exercise 2")
  (p (run* [q] (appendo [1 2 3] [4 5 6] q)))
  ;; ((1 2 3 4 5 6))
  (p (run* [q] (appendo [1 2 3] q [1 2 3 4 5 6])))
  ;; ((4 5 6))
  ;; should be a failure
  (p (run* [q] (appendo q [1 2 3] [1 2 3 4 5 6])))
  ;; ()
  (p (run* [q] (appendo q [1 2 3] [1 2 3 1 2 3])))
  ;; ((1 2 3))
  )
