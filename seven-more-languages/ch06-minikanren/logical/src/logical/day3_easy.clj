(ns logical.day3-easy)

(use 'clojure.core.logic)
(use 'logical.utils)

(require '[clojure.core.logic.fd :as fd])

(defn day3-easy
  []
  (p "day 3 - do easy")
  ;; let's find pythagorean triples
  (p
   (run* [a b c]
     (fd/in a b c (fd/interval 1 100))
     (fd/<= a b)
     (fd/<= b c)
     (fd/eq
      (= (+ (* a a) (* b b)) (* c c)))))
  ;; some examples to see "conde" working in action
  ;; every point within [0:10,0:10] that satisfies
  ;; either x + y == 10 or x == y
  (p
   (run* [a b]
     (fd/in a b (fd/interval 0 10))
     (conde
      [(fd/eq
        (= a b))]
      [(fd/eq
        (= (+ a b) 10))]))))
