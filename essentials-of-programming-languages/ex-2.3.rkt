#lang eopl

(require "./common.rkt")

(define (diff-tree->int t)
  (if (equal? t '(one))
    1
    (- (diff-tree->int (cadr t))
       (diff-tree->int (caddr t)))))

(define (int->diff-tree x)
  (cond ((= x 0)
          (zero))
        ((< x 0)
          (predecessor (int->diff-tree (+ x 1))))
        ((> x 0)
          (successor (int->diff-tree (- x 1))))))

(define (zero) '(diff (one) (one)))

(define (is-zero? t)
  (= 0 (diff-tree->int t)))

(define (successor t)
  ; t+1 = t-(-1)
  ; -1 = predecessor of zero
  (define neg-one
    (predecessor (zero)))
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

(define (neg a)
  ; if a = (one), -a = -1
  ; else a = (diff <l> <r>), -a = (diff <r> <l>)
  (if (equal? a '(one))
    (predecessor (zero))
    (list 'diff
          (caddr a)
          (cadr a))))

(define (diff-tree-plus a b)
  ; a + b = a - (-b)
  ; (neg x) runs in constant time
  (list 'diff
        a
        (neg b)))

(define (test-diff-tree-plus a b)
  (out "result:"
       (diff-tree->int
         (diff-tree-plus
           (int->diff-tree a)
           (int->diff-tree b)))
       "expected:"
       (+ a b)))

(test-diff-tree-plus 1 10)
(test-diff-tree-plus 1 -1)
(test-diff-tree-plus 30 20)
(test-diff-tree-plus -30 20)
