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

(define (stream-enumerate-inteval low high)
  (if (> low high)
    nil
    (cons-stream low (stream-enumerate-inteval
                       (+ low 1)
                       high))))

(define (prime-computation)
  (out 
    (stream-car
      (stream-cdr
        (stream-filter
          prime?
          (stream-enumerate-inteval
            10000 1000000))))))

(prime-computation)

(end-script)
