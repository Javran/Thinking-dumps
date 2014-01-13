#lang eopl

(require "../common.rkt")

; interfaces:
; * zero
; * is-zero?
; * successor
; * predecessor

(define baseN 16)

(define (zero) '())

(define is-zero? null?)

(define (successor n)
  (if (null? n)
    ; the successor of zero is one
    (cons 1 (zero))
    ; else
    (let ((next-least-bigit (+ (car n) 1)))
      (if (>= next-least-bigit baseN)
        ; this bigit will overflow `baseN`
        (cons 0 (successor (cdr n)))
        ; else
        (cons next-least-bigit (cdr n))))))

(define (predecessor n)
  (if (null? n)
    ; predecessor of zero is not specified
    (eopl:error
      'predecessor
      "not specified")
    ; else
    (let ((next-least-bigit (- (car n) 1)))
      (if (< next-least-bigit 0)
        ; must be 0-1 = -1 < 0
        (cons (- baseN 1) (predecessor (cdr n)))
        ; else
        (if (and (= next-least-bigit 0)
                 (null? (cdr n)))
          ; if rest parts are all zeros
          '()
          ; else
          (cons next-least-bigit (cdr n)))))))

(out "successor:")
(define n-37
  (let loop ((n (zero))
             (i 0))
    (out n)
    (if (< i 36)
      (loop (successor n) (+ i 1))
      n)))

(out "predecessor:")
(let loop ((n n-37)
           (i 0))
  (out n)
  (if (< i 36)
    (loop (predecessor n) (+ i 1))
    n))

(define (add x y)
  (if (is-zero? x)
    y
    (successor (add (predecessor x) y))))

(define (mul x y)
  (if (is-zero? x)
    (zero)
    (add (mul (predecessor x) y) y)))

(define (factorial n)
  (if (is-zero? n)
    ; 0! = 1
    (successor (zero))
    ; n! = (n-1)! * n
    (mul (factorial (predecessor n)) n)))

(define (int->bignum x)
  (if (= x 0)
    (zero)
    (successor (int->bignum (- x 1)))))

(out (factorial (int->bignum 7)))

; execution time grows exponentially as the argument increases
;   let T(n) be the total time required for (factorial (int->bignum n)), then
;   T(0) = 1
;   T(n) = T(n-1) * n
;
; the change of base does not have significant effect on execution time,
;   because `successor`, `predecessor`, `is-zero?` can be done in constant time.
; however, I guess bigger base number will slightly speed up the computation,
;   because of less overhead of list operation.
