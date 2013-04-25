(load "../common/utils.scm")

; definition of Fibonacci numbers:
; fib 0 = 0
; fib 1 = 1
; fib n = fib (n-1) + fib (n-2)

; fibonacci
(define (fib-1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ; note fib-1 calls itself twice here
        (else (+ (fib-1 (- n 1))
                 (fib-1 (- n 2))))))
; defect: redundant computation
; to calculate fib 40, fib 39 and fib 38 is called,
; to calculate fib 39, fib 38 and fib 37 is called,
; ...
; so fib 38 is called at least 2 times, each time it need to do the same task
; when fib is applied with a big number, the time consumption might not be affordable.

(define (print-func func testcases)
  (out "== Print Begin ==")
  (for-each
    (lambda (x)
      (display x)
      (display ":\t")
      (out (func x)))
    testcases)
  (out "== Print End =="))
(time-test print-func fib-1 '(0 1 2 3 4 5 6 7 8 9 20))

(define (fib-2 n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 ; fib 1 = 1
            0 ; fib 0 = 0
            n))
(time-test print-func fib-2 '(0 1 2 3 4 5 6 7 8 9 20))
