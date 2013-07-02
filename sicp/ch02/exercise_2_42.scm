(load "../common/utils.scm")

; coordinate: (cons row col)
;   note: row and col start from 1
; a queen placement: a list of coordinate, each stands for a col

; an empty-board, from the definition, is `nil`
(define empty-board nil)

(define (adjust-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define (list-member? x ls)
  ; x is a member of ls?
  (if (null? ls)
    #f
    (if (= x (car ls))
      #t
      (list-member? x (cdr ls)))))

(out (list-member? 3 (list 1 2 3)))
; #t
(out (list-member? 4 (list 1 2 3)))
; #f
(newline)

(define (safe? k positions)
  (let ((safe-positions (cdr positions))
        ; already knowing that the first element is (cons row k)
        ; and we only have interest in `row`
        (cur-row (caar positions)))
    (not 
      (list-member? 
        cur-row
        (flatmap
          ; for each safe position
          ;   figure out rows that the queen can move to in column k
          (lambda (pos)
            ; returns all rows that the position can move to in col k
            (let ((row (car pos))
                  (col (cdr pos)))
              (list row
                    (+ row (- k col))
                    (- row (- k col)))))
          safe-positions)))))

(out (safe? 2 '( (2 . 2) (1 . 1) )))
; #f
(out (safe? 2 '( (3 . 2) (1 . 1) )))
; #t
(newline)

(define (queens board-size)
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

; it's not exactly the placement but is a way of quick visualization
(define (pretty-print-board board)
  (define board-size (length board))
  (define (pretty-print-line pos)
    (let ((row (car pos)))
      (for-each
        display
        (map (lambda (x)
               (if (= x row)
                 #\X
                 #\.))
             (list-in-range 1 board-size)))
      (newline)))
  (for-each
    pretty-print-line
    board)
  (newline))


(define solutions (queens 8))

; only visualize the first solution
(pretty-print-board (car solutions))

;(for-each
;  pretty-print-board
;  solutions)

(out "Solution count: "
     (length solutions))

(end-script)
