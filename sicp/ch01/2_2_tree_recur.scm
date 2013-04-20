(load "../common/utils.scm")

; fibonacci
(define (fib-1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-1 (- n 1))
                 (fib-1 (- n 2))))))

(define (print-func func testcases)
  (out "== Print Begin ==")
  (for-each
    (lambda (x)
      (display x)
      (display ":\t")
      (out (func x)))
    testcases)
  (out "== Print End =="))
(print-func fib-1 '(0 1 2 3 4 5 6 7 8 9))

(define (fib-2 n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
(print-func fib-2 '(0 1 2 3 4 5 6 7 8 9))
