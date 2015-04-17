;; tests in the original metacircular should be moved to here
;; and instead of having more fine-grained tests
;; we will just test the behavior: if our metacircular evaluator
;; gives the same result as the one given by the evaluator in our implementing
;; language, then it is fine.

(define test-exprs
  '(
    (+ 1 2 3)
    (- 7 1 2 3)
    (/ 1024 256)
    (= 1 1)
    (= 1 0)
    (zero? 0)
    (eq? 'a 'a)
    (eq? 'a 'b)
    (= (* 1 2 3 4) (* 2 (+ 10 2)))
    (cons 1 '(2 3))
    (cons 'a 'b)
    (car '(1 2))
    (cdr '(1 2))
    (null? '())
    (null? '(1))
    (list 'a 'b)
    ))
