(load "../common/utils.scm")

; row: 1,2,3...
; col: 1,2,3...
; pascal _ 1 = 1
; pascal row row = 1
; pascal row col = pascal (row-1) col + pascal (row-1) (col-1)
(define (pascal row col)
  (cond ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1)
                         col)
                 (pascal (- row 1)
                         (- col 1))))))

(let loop-row ((i 1))
  (if (<= i 10)
    (begin
      (let loop-col ((j 1))
        (if (<= j i)
          (begin
            (display (pascal i j))
            (display " ")
            (loop-col (+ j 1)))
          (newline)))
      (loop-row (+ i 1)))))
