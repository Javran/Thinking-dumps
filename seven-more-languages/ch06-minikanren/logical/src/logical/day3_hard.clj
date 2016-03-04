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

(defn solve-sudoku
  [puzzle]
  ;; get cell value from symbol
  (defn get-cell
    [sym]
    (let [s (name sym)
          row (- (int (get s 1)) (int \0))
          col (- (int (get s 2)) (int \0))]
      (get (get puzzle row) col)))
  (run* [c00 c01 c02 c03 c04 c05 c06 c07 c08
         c10 c11 c12 c13 c14 c15 c16 c17 c18
         c20 c21 c22 c23 c24 c25 c26 c27 c28

         c30 c31 c32 c33 c34 c35 c36 c37 c38
         c40 c41 c42 c43 c44 c45 c46 c47 c48
         c50 c51 c52 c53 c54 c55 c56 c57 c58

         c60 c61 c62 c63 c64 c65 c66 c67 c68
         c70 c71 c72 c73 c74 c75 c76 c77 c78
         c80 c81 c82 c83 c84 c85 c86 c87 c88]
    (doseq [row (range 0 9)
            col (range 0 9)]
      (let [sym (symbol (str "c" row col))]
        (eval
         `(fd/in ~sym (fd/interval 0 10)))))))

(defn day3-hard
  []
  (p "day 3 - do hard")
  (p "exercise 1")
  ;; remember that when we were dealing with prolog in
  ;; "Seven Languages in Seven Weeks", we have to list all variables
  ;; in order to write constraints done,
  ;; I don't know if we can do something better,
  ;; but let's first get it to work.
  (p (solve-sudoku
      (parse-sudoku
       ["1___"
        "_21_"
        "__3_"
        "___4"]))))
