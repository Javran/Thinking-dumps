(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define fact
  (lambda (n)
    ((lambda (fact) (fact fact n))
     (lambda (ft k)
       (if (= k 0)
         1
         (* k (ft ft (- k 1))))))))

(do-test
  fact
  (list
    (mat 0 1)
    (mat 1 1)
    (mat 2 2)
    (mat 4 24)
    (mat 10 3628800)))
(newline)

(define fib
  (lambda (n)
    ((lambda (fib) (fib fib n))
     (lambda (ft k)
       (if (<= k 1)
         k
         (+ (ft ft (- k 1))
            (ft ft (- k 2))))))))

(out (fib 4))




(end-script)
