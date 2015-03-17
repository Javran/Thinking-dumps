;; some additional tests for exercise  5.43

(define test-exps-ex-5-43
  '(1
    'a
    (let ()
      1)
    (let ()
      (define x 10)
      (define y 20)
      (define k (+ 2 x))
      (+ k x y))
    ;; test for coverage
    (begin
      ;; definition/variable
      (define x 10)
      ;; variable
      x
      ;; begin
      (begin
        (define y
          (if #t
              20
              40))
        ;; assignment
        (set! y 30))
      ;; definition/function
      ;; we have not yet considered the case
      ;; where improper lists are used in lambda
      ;; arguments
      (define (f u v)
        (* u v))
      ;; explicit lambda
      (define t (lambda (x) x))
      ;; application
      (+ (t x)
         ;; cond
         (cond (#f (t y))
               (else (t (t y))))
         ;; let
         (let ()
           (define k (+ 2 x))
           (+ k (f x x)))))
    ;; ====
    ((lambda ()
       (define x 1)
       (define y 2)
       (+ x y)))
    ;; ====
    ((lambda (x)
       (begin
         (define y 1)
         (define z 2)
         (if (= x 0)
             (let ()
               (define a 10)
               (+ x a))
             (let ()
               (define b 320)
               (* x 3 b)))))
     1234)
    ;; ====
    (begin
      (define f (lambda (x)
                  (lambda (y)
                    (+ x x y))))
      ((f 10) 20))
    ;; ====
    (begin
      (define (f x y) (+ x y))
      (let ()
        (define k (+ 2 10))
        (+ k (f 10 10))))
    ))

(set! test-exps
      (append test-exps test-exps-ex-5-43))
