(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_70_common.scm")

(define order-a ((curry2 apply) +))

(define (order-b xs)
  (let ((i (car xs))
        (j (cadr xs)))
    (+ (* 2 i)
       (* 3 j) 
       (* 5 i j))))

(define (valid xs)
  (define (divisible x y)
    (= 0 (remainder x y)))
  (define (not-divisible-by x ys)
    (if (null? ys)
      #t
      (and (not (divisible x (car ys)))
           ; short-circuit
           (not-divisible-by x (cdr ys)))))
  (and (not-divisible-by (car xs) '(2 3 5))
       (not-divisible-by (cadr xs) '(2 3 5))))

; a. ordered according to the sum `i+j`
(define stream-a
  (weighted-pairs integers integers order-a))

(print-few 20 stream-a)

; b. ordered according to the sum `2i+3j+5ij`,
;  neither i nor j is divisible by 2,3 or 5.
(define stream-b
  (stream-filter
    valid
    (weighted-pairs integers integers order-b)))

; see "./exercise_3_70_verify.hs" for verification

(print-few 20 stream-b)

(end-script)
