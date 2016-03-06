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

(defn parse-sudoku
  [rawvec]
  (defn parse-char
    [ch]
    (if (and (<= (int \1) (int ch))
             (<= (int ch) (int \9)))
      (- (int ch) (int \0))
      nil))
  (vec (map #(vec (map parse-char %)) rawvec)))

(def puzzle-raw
  ["5____2__6"
   "9____1_31"
   "_62__19__"
   "______892"
   "_79___41_"
   "123______"
   "__61__52_"
   "23_7____9"
   "7__5____4"])

(defn transpose [m]
  (apply mapv vector m))

(defn day3-hard
  []
  (p "day 3 - do hard")
  (p "exercise 1")
  (let [puzzle-vars (vec
                (repeatedly 9
                 (fn []
                   (vec (repeatedly 9 lvar)))))
        all-vars (apply concat puzzle-vars)
        rows puzzle-vars
        cols (transpose puzzle-vars)
        grids (apply concat
                     (mapv (fn [group]
                             (mapv (fn [x]
                                     (vec (apply concat x)))
                                   (transpose
                                    (map #(partition 3 %) group))))
                           (partition 3 puzzle-vars)))
        puzzle (parse-sudoku puzzle-raw)
        zipped-puzzle (map list
                           (apply concat puzzle)
                           (apply concat puzzle-vars))]
    ;; without any constraint, we will get a valid sudoku
    (p (run 1 [q]
         (== q puzzle-vars)
         (everyg #(fd/in % (fd/interval 1 9)) all-vars)
         (everyg (fn [ [v lv] ]
                   (if (nil? v)
                     succeed
                     (fd/eq (= v lv))))
                 zipped-puzzle)
         (everyg fd/distinct rows)
         (everyg fd/distinct cols)
         (everyg fd/distinct grids)
         ))
    ))
