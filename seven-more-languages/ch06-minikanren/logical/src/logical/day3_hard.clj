(ns logical.day3-hard)

(use 'clojure.core.logic)
(use 'logical.utils)

(require '[clojure.core.logic.fd :as fd])

(defn parse-sudoku
  [rawvec]
  (defn parse-char
    [ch]
    (if (and (<= (int \1) (int ch))
             (<= (int ch) (int \9)))
      (- (int ch) (int \0))
      nil))
  (vec (map #(vec (map parse-char %)) rawvec)))

(defn day3-hard
  []
  (p "day 3 - do hard")
  (p "exercise 1")
  (p (parse-sudoku
      ["1___"
       "_21_"
       "__3_"
       "___4"])))
