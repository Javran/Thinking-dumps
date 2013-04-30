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

(define (fast-prime? n times)
  (if (= times 0)
    (cons #t 0)
    (let ((result (tracked-fermat-test n)))
      (if (car result)
        (let ((sub-result (fast-prime? n (- times 1))))
          (cons (car sub-result) 
                (+ (cdr sub-result) (cdr result))))
        ; else
        (cons #f (cdr result))))))

(out (fast-prime? 2147483647 1))
; (#t . 308)
