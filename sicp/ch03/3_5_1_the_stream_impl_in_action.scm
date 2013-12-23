(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (prime? n)
  ; assume n >= 2
  (define (smallest-divisor n)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (square x) (* x x))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n) ; impossible
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (stream-enumerate-interval low high)
  (if (> low high)
    nil
    (cons-stream low (stream-enumerate-interval
                       (+ low 1)
                       high))))

(define (prime-computation)
  (out 
    (stream-car
      (stream-cdr
        (stream-filter
          prime?
          (stream-enumerate-interval
            10000 1000000))))))

(prime-computation)

(define (my-stream-filter pred stream)
  ; here even if the part
  ;   `(my-stream-filter pred (stream-cdr stream)`
  ; is the same, we cannot factor it out simply
  ; because if we write it in a `let`, we cannot
  ; control when it will be evaluated.
  ; a workaround to get rid of redundant code
  ; will be better.
  (cond ((stream-null? stream)
          the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (my-stream-filter
                         pred
                         (stream-cdr stream))))
        (else
          (my-stream-filter
            pred
            (stream-cdr stream)))))

(define (stream->list s)
  (if (stream-null? s)
    nil
    (cons (stream-car s)
          (stream->list (stream-cdr s)))))

(define (my-stream-filter-test)
  (out "result from stream:")
  (out (stream->list
       (my-stream-filter
         prime?
         (stream-enumerate-interval
           10000
           10100))))
  (out "result from list:")
  (out (filter
         prime?
         (list-in-range
           10000
           10100))))

; uncomment to see the test
(my-stream-filter-test)

(end-script)
