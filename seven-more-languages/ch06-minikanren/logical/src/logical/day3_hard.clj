(ns logical.day3-hard)

(use 'clojure.core.logic)
(use 'logical.utils)

(require '[clojure.core.logic.fd :as fd])

;; If you don't know how to do this,
;; it's not your fault.
;; core.logic is missing so much documents
;; that I can't find out anything useful to accomplish our task.
;; one clue: https://gist.github.com/swannodette/3217582
;; I will be writing my own according to the code above.

(defn transpose [m]
  (apply mapv vector m))

(defn day3-hard
  []
  (p "day 3 - do hard")
  (p "exercise 1")
  (let [puzzle (vec
                (repeatedly 9
                 (fn []
                   (vec (repeatedly 9 lvar)))))
        rows puzzle
        cols (transpose puzzle)
        grids (partition 3 puzzle)]
    (p rows)
    (p cols)
    (p (run 5 [q]
         (== q cols)))))
