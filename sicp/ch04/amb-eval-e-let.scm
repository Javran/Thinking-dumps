(load "./my-eval-e-let.scm")
(load "./amb-eval-test.scm")

(define (install-amb-let)

  (define (analyze-let exp)
    (amb-analyze (let->combination exp)))

  (define (test)
    (let ((env (init-env)))
      ;; functionality of "let"
      (do-test
       test-eval
       (list
        ;; test normal let
        (mat `(let ((x 1)
                    (y 2)
                    (z 3))
                (+ x y z)) env 6)
        (mat `(let ((a 10)
                    (b 20))
                (+ a a)
                (* b a)) env 200)
        (mat `(let ()
                10) env 10)
        (mat `(let ((a 10))
                (let ((a (+ a 20)))
                  (let ((a (* a 30)))
                    (+ a a)))) env 1800)
        ;; test named let
        (mat `(let fib-iter ((a 1) (b 0) (count 10))
                (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))) env 55)
        (mat `(let fib-iter ((a 1) (b 0) (count 14))
                (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))) env 377)
        (mat `(let proc ((i 1) (acc 1))
                (if (<= i 10) (proc (+ i 1) (* i acc)) acc)) env 3628800)
        (mat `(let proc ((i 1) (acc 0))
                (if (<= i 100) (proc (+ i 1) (+ i acc)) acc)) env 5050)
        )
       (test-compare equal?))

      ;; functionality of "amb"
      (do-test
       amb-eval-all
       (list
        (mat `(let ((x (amb 1 2))
                    (y 10))
                (+ x y))
             env '(11 12))
        (mat `(let ((x (amb #t #f))
                    (y #f))
                (if x
                    (set! y (lambda (a) (+ a 1)))
                    (set! y (lambda (a) (- a 1))))
                (y 10))
             env '(11 9))
        ))

      'ok))

  (define handler
    (make-amb-handler
     'let
     analyze-let
     test))

  (ahandler-register! handler)
  'ok)
