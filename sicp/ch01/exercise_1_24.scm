(load "../common/utils.scm")

; again we need some modification to keep track of the time consumption
;     in place of "timed-prime-test"
(define (tracked-expmod-i base count m)
  (define (expmod-iter base count m n) ; constant: (base^count * n) mod m
    (cond ((= count 0) 
            ; count = 0, (base^count * n) mod m = n mod m
            ; consumption: cond(1)+remainder(1)
            (cons (remainder n m) 3))
          ((even? count)
            ; (base^count * n) mod m = ((base^2)^(count/2) * n) mod m
            (let ((result (expmod-iter (remainder (square base) m) (/ count 2) m n)))
              ; consumption: cond(1+1)+square(1)+remainder(1)+`/`(1)
              (cons (car result) (+ (cdr result) 5))))
          (else
            ; consumption: cond(2)+`-`(1)+remainder(1)+`*`(1)
            (let ((result (expmod-iter base (- count 1) m (remainder (* base n) m))))
              (cons (car result) (+ (cdr result) 5))))))
  (expmod-iter base count m 1))

(define (tracked-fermat-test n)
  (define (try-it a)
    (let ((result (tracked-expmod-i a n n)))
      (cons (= (car result) a) (cdr result))))
  (try-it (random-range-in 1 (- n 1))))

(define (tracked-fast-prime? n times)
  (if (= times 0)
    (cons #t 0)
    (let ((result (tracked-fermat-test n)))
      (if (car result)
        (let ((sub-result (tracked-fast-prime? n (- times 1))))
          (cons (car sub-result) 
                (+ (cdr sub-result) (cdr result))))
        ; else
        (cons #f (cdr result))))))

(out (tracked-fast-prime? 2147483647 1))
; (#t . 308)

; f(n) = c * log(n)/log(2)
; c = f(n) /(log(n)/log(2)) should be relatively stable
; assmue when n = 1000, consumption = f(1000) = c1 = c * log(1000)/log(2)
; then:
;     f(1000000) = c2 = c * log(1000000)/log(2) = 2 * c * log(1000)/log(2) = 2 * c1
(define (verbose-fast-prime-test n)
  (let ((result (tracked-fast-prime? n 5)))
    (display "input: ")
    (display n)
    (newline)
    (display "output: ")
    (display result)
    (newline)
    (display "f(n):(log(n)/log(2))= ")
    (display (exact->inexact 
               (/ (cdr result)
                  (/ (log n) (log 2)))))
    (newline)))

(for-each
  verbose-fast-prime-test
  '(    1009    1013    1019
       10007   10009   10037
      100003  100019  100043
     1000003 1000033 1000037))
; we can see that:
;     for n = 1009,    counter = 415
;     for n = 1000003, counter = 715
; c1 : c2 = 415 / 715 = 0.58 
; and the constant c is roughly within range:(35,47) 

; TODO:
; actually I don't think we have to track the consumption manually ...
; we can run fermat test with a big 'time' so the time consumption can be observed directly ...
