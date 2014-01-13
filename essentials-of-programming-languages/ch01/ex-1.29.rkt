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

; sort-1 :: [Int] -> [Int]
; usage: sort `loi` in ascending order
(define (sort-1 loi)
  (if (<= (length loi) 1)
    loi
    (merge (list (car loi))
           (sort-1 (cdr loi)))))

; quick-sort :: [Int] -> [Int]
; usage: sort `loi` in ascending order
(define (quick-sort loi)
  (if (null? loi)
    '()
    (let ((pivot (car loi)))
      (append (quick-sort (filter (lambda (x) (<= x pivot)) (cdr loi)))
              (list pivot)
              (quick-sort (filter (lambda (x) (>  x pivot)) (cdr loi)))))))

(out (sort-1 '(8 2 5 2 3))
     (sort-1 '(0 1 9 2 8 3 7 4 6 5))
     (quick-sort '(8 2 5 2 3))
     (quick-sort '(0 1 9 2 8 3 7 4 6 5))
     )
