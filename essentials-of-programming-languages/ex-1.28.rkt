#lang eopl

(require "./common.rkt")

; merge :: ([Int], [Int]) -> [Int]
; usage: merge two sorted (ascending) lists of integers into sorted one
(define (merge loi1 loi2)
  (cond ((null? loi1) loi2)
        ((null? loi2) loi1)
        (else
          (if (<= (car loi1) (car loi2))
            (cons (car loi1) (merge (cdr loi1) loi2))
            (cons (car loi2) (merge loi1 (cdr loi2)))))))

(out (merge '(1 4) '(1 2 8))
     (merge '(35 62 81 90 91) '(3 83 85 90))
     (merge '(1 4 7) (merge '(2 5 8) '(3 6 9)))
     )
