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
;; TODO: doc about functions that might be useful

(defn parse-sudoku
  "parse a list of raw lines of one sudoku
  to a 2-d vector containing only number 1-9 and nil"
  [rawvec]
  (defn parse-char
    [ch]
    (if (and (<= (int \1) (int ch))
             (<= (int ch) (int \9)))
      (- (int ch) (int \0))
      nil))
  (vec (map #(vec (map parse-char %)) rawvec)))

(def puzzle-example-raw
  ["5____2__6"
   "9____5_31"
   "_62__19__"
   "______892"
   "_79___41_"
   "123______"
   "__61__52_"
   "23_7____9"
   "7__5____4"])

(defn transpose [m]
  (apply mapv vector m))

(defn solve-sudoku
  "return a stream of all possible sudoku solutions"
  [puzzle]
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
        ;; flatten both puzzle cells and corresponding logic variables
        ;; TODO: length check
        zipped-puzzle (map list
                           (apply concat puzzle)
                           (apply concat puzzle-vars))]
    (run* [q]
      (== q puzzle-vars)
      (everyg (fn [ [v lv] ]
                ;; if this cell does not have a concrete value?
                (if (nil? v)
                  ;; assign lvar with a possible range
                  (fd/in lv (fd/interval 1 9))
                  ;; or assign lvar with the concrete value
                  (== lv v)))
              zipped-puzzle)
      ;; sudoku requirements, distinct rows, cols and grids
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct grids)
      )))

(defn day3-hard
  []
  (p "day 3 - do hard")
  (p "exercise 1")
  ;; without any constraint, we will get a valid sudoku
  (p (first (solve-sudoku (repeat 9 (repeat 9 nil)))))
  ;; test on real sudoku puzzle
  (p (first (solve-sudoku (parse-sudoku puzzle-example-raw)))))

