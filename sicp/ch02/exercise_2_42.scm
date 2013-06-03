(load "../common/utils.scm")

(define (queens borad-size)
  ; solution in first k-th cols
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        ; `positions` should be the queens' placement
        ;   check if the k-th queen is safe
        (lambda (positions)
          (safe? k positions))
        (flatmap
          ; rest-of-queens here stands for a valid solution
          ;   for the first (k-1)-th queens
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjust-position new-row
                                    k
                                    rest-of-queens))
                 ; try new-row from 1 to board-size
                 (enumerate-interval 1 board-size)))
          ; solution of the first (k-1)-th queens
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(end-script)
