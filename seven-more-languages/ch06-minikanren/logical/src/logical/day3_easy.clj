(ns logical.day3-easy)

(use 'clojure.core.logic)
(use 'logical.utils)

(require '[clojure.core.logic.fd :as fd])

(defn day3-easy
  []
  (p "day 3 - do easy")
  (p
   (run* [q]
     ;; q <= 1 and q in range 0~10
     (fd/in q (fd/interval 0 10))
     (fd/<= q 1))))
