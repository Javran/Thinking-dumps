;; tests in the original metacircular should be moved to here
;; and instead of having more fine-grained tests
;; we will just test the behavior: if our metacircular evaluator
;; gives the same result as the one given by the evaluator in our implementing
;; language, then it is fine.

;; TODO: I think we can just use ec-tests.
;; as we don't necessarily need a complete set of testcases.

(define test-exprs
  '(
    ;; self-evaluating & quotation
    'abc
    1
    '(1 2 3)
    "a"
    #\a
    '"a"
    '1
    ;; if-expression
    (if 1 10)
    (if 1 10 20)
    (if #t 10 20)
    (if 'a 10 20)
    (if #f 10 20)
    (if (= 0 1) (+ 10 20) (* 10 20))

    ;; begin-form
    (begin (if #f 10 20) 30)
    (begin 1 2 3)
    (begin 30 (if #t 10 20))

    ;; cond-form
    (begin
      (define a 0)
      (cond ((= a 0) 2)
            ((= a 1) 1)
            ((= a 2) 0)))
    (begin
      (define a 2)
      (cond ((= a 0) 2)
            ((= a 1) 1)
            ((= a 2) 0)))
    (begin
      (define a 1)
      (cond ((= a 0) => (lambda (x) (if x 10 20)))
            ((= a 1) => (lambda (x) (if x 30 40)))
            ((= a 2) 50)
            (else 60)))
    (begin
      (define a 3)
      (cond ((= a 0) => (lambda (x) (if x 10 20)))
            ((= a 1) => (lambda (x) (if x 30 40)))
            ((= a 2) 50)
            (else 60)))
    ;; env functionality
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
