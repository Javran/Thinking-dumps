(load "../common/utils.scm")

; accumulate - recursive version
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate-rec combiner null-value term (next a) next b))))

; accumulate - iteractive version
(define (accumulate-itr combiner null-value term a next b)
  (define (accumulate-iter a acc)
    (if (> a b)
      acc
      (accumulate-iter (next a) (combiner (term a) acc))))
  (accumulate-iter a null-value))

(define (make-sum accumulate a b)
  (accumulate + 0 identity a inc b))

(define (make-product accumulate a b)
  (accumulate * 1 identity a inc b))

(out (make-sum accumulate-rec 1 100))
(out (make-sum accumulate-itr 1 100))
; 5050
(out (make-product accumulate-rec 1 10))
(out (make-product accumulate-itr 1 10))
; 3628800
