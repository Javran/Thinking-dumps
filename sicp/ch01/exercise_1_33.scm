(load "../common/utils.scm")

; filtered-accumulate - recursive version
(define (accumulate-rec combiner null-value term a next b predicate)
  (if (> a b)
    null-value
    (let ((rest-results (lambda () 
                          (accumulate-rec
                            combiner
                            null-value 
                            term 
                            (next a) 
                            next 
                            b
                            predicate))))              
      (if (predicate a)
        (combiner (term a) (rest-results))
        (rest-results)))))

; filtered-accumulate - iteractive version
(define (accumulate-itr combiner null-value term a next b predicate)
  (define (accumulate-iter a acc)
    (if (> a b)
      acc
      (accumulate-iter (next a)
                       (if (predicate a)
                         (combiner (term a) acc)
                         acc))))
  (accumulate-iter a null-value))

(out (accumulate-rec + 0 identity 1 inc 100 odd?))
(out (accumulate-itr + 0 identity 1 inc 100 odd?))
; 2500

(out (accumulate-rec + 0 identity 1 inc 100 even?))
(out (accumulate-itr + 0 identity 1 inc 100 even?))
; 2550

(out (accumulate-rec * 1 identity 1 inc 10 odd?))
(out (accumulate-itr * 1 identity 1 inc 10 odd?))
; 945

(out (accumulate-rec * 1 identity 1 inc 10 even?))
(out (accumulate-itr * 1 identity 1 inc 10 even?))
; 3840

(out (* 945 3840))
; 3628800

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (square x) (* x x))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n) ; impossible
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (if (< n 2)
    #f
    (= n (smallest-divisor n))))

(out (filter prime? (list-in-range 1 100)))
; print primes less than 100

(define (sum-of-square-of-prime-in-range a b)
  (accumulate-itr + 0 square a inc b prime?))

(out (sum-of-square-of-prime-in-range 20 500))

; verification
(out (apply + 
            (map square 
                 (filter prime? 
                         (list-in-range 20 500)))))
