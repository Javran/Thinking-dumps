#lang eopl

(require "./common.rkt")

; number-elements :: [a] -> [(Int,a)]
; usage: `zip [1..] lst`
(define (number-elements lst)
  ; increase the index contained in each element of `rest-lst`
  ;   and attach `first-lst` in front of `rest-lst` 
  (define (merge-list first-lst rest-lst)
    (cons first-lst
          (map (lambda (xs)
                 (cons (+ 1 (car xs))
                       (cdr xs)))
               rest-lst)))
  (define g merge-list)
  (if (null? lst)
    '()
    (g (list 0 (car lst))
       (number-elements (cdr lst)))))

(out (number-elements '(a b c d e f g)))
