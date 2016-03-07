(ns logical.day3-hard)

(use 'clojure.core.logic)
(use 'logical.utils)

(require '[clojure.core.logic.fd :as fd])
(use '[clojure.string :only (join)])

;; If you don't know how to do this,
;; it's not your fault.
;; core.logic is missing so much documents
;; that I can't find out anything useful to accomplish our task.
;; one clue: https://gist.github.com/swannodette/3217582
;; I will be writing my own according to the code above.

;; some notes that might be useful for the exercise:
;; - lvar & lvars
;; one of the problem I'm facing is that I don't know
;; how to create logic variables without using "fresh" or "run"
;; and the answer is "lvar". it creates one logic variable at a time
;; (you can also use "(lvars n)" to create a list of them)
;; - (do (foo) (bar) ... succeed)
;; this might be useful for debugging: if you find yourself
;; want to print something inside the block of "run",
;; you might get "NullPointerException" errors.
;; I don't know what exactly goes wrong, but it seems that "run"
;; (and probably "fresh") expect a sequence of expressions that returns
;; something "like a constraint" -- and the obviously choice is "succeed".
;; this trick works fine for me, I'll recommend using it to print out
;; some intermediate values for debugging.

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
        zipped-puzzle
        (let [ps (apply concat puzzle)
              vs (apply concat puzzle-vars)]
          (assert (== (count ps) (count vs))
                  "puzzle and lvar's count mismatch")
          (map list ps vs))]
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

(defn print-puzzle
  [puzzle]
  (let [horiz-sep "---+---+---"
        cell-to-char (fn [x] (if (nil? x) "_" (str x)))
        three-rows
        (apply concat
               (interpose
                ;; insert separator
                (list horiz-sep)
                ;; seq of things like "123|456|789" (3 of them)
                (map (fn [trs]
                       (map (fn [row]
                              (join "|"
                                    ;; ("123" "456" "789")
                                    (map #(apply str %)
                                         (partition 3 (map cell-to-char row)))))
                            trs))
                     (partition 3 puzzle))))]
    (doseq [l three-rows]
      (p l)))
  (p ""))

(defn day3-hard
  []
  (p "day 3 - do hard")
  (p "exercise 1")
  ;; without any constraint, we will get a valid sudoku
  (print-puzzle (first (solve-sudoku (repeat 9 (repeat 9 nil)))))
  ;; test on real sudoku puzzle
  (print-puzzle (first (solve-sudoku (parse-sudoku puzzle-example-raw))))
  ;; skipping the last exercise because I'm not interested in making up stories.
  ;; sorry.
  )
