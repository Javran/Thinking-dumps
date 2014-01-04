#lang eopl

(require "./common.rkt")

(define (diff-tree->int t)
  (if (equal? t '(one))
    1
    (- (diff-tree->int (cadr t))
       (diff-tree->int (caddr t)))))

(define (zero) '(diff (one) (one)))

(define (is-zero? t)
  (= 0 (diff-tree->int t)))

(define (successor t)
  ; t+1 = t-(-1)
  (define neg-one
    (list 'diff (zero) '(one)))
  (list 'diff t neg-one))

(define (predecessor t)
  ; t-1
  (list 'diff t '(one)))

; test representation
; second round, from 10 to 0
(let loop ((n
             ; first round, from 0 to 10
             (let loop ((n (zero))
                        (i 0))
               (out (diff-tree->int n))
               (if (< i 10)
                 (loop (successor n) (+ i 1))
                 n)))
           (i 0))
  (out (diff-tree->int n))
  (if (< i 10)
    (loop (predecessor n) (+ i 1))
    n))
