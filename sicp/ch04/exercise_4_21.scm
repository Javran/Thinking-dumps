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

(do-test
  fib
  (list
    (mat 0 0)
    (mat 1 1)
    (mat 2 1)
    (mat 4 3)
    (mat 10 55)))
(newline)

(define even?-native even?)

(define even? #f)

(define (even? x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))

(define test-inputs
  (list-in-range 0 10))

(do-test
  even?
  (map mat test-inputs
           (map even?-native test-inputs)))

(end-script)
