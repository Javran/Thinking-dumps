;;; optional module for ec that provides some test facilities

;; evaluate the expression using both the machine evaluator
;; and the real scheme evaluator under the default environment settings
;; for optional arguments, see:
;; http://web.mit.edu/scheme_v9.0.1/doc/mit-scheme-ref/Lambda-Expressions.html
(define (test-evaluator machine-eval #!optional verbose)
  (lambda (exp)
    ;; verbose = true by default
    (let ((machine-result
           (machine-eval exp (init-env)))
          (lisp-result
           (eval exp (make-top-level-environment))))
      (if (equal? machine-result lisp-result)
          (if (or (default-object? verbose) verbose)
              (display ".")
              'done)
          (error "eval results are inconsistent:"
                 "expecting" lisp-result
                 "while getting" machine-result)))))

(define test-exps
  `(
    ;; test self-evaluating
    10
    1.3
    "wow"
    #\space
    #\a
    #t
    ;; test variable
    (begin
      (define x +)
      (x 1 2 3))
    (begin
      (define x 10)
      (define y 20)
      (- x y))
    ;; test quoted
    (quote ())
    '(a b c)
    'a
    '"wow"
    '(a (b c) (d e))
    ;; test assignment
    (begin
      (define y 10)
      (set! y 20)
      (set! y (+ y y))
      y)
    ;; test definition
    (begin
      ;; tested already
      (define a 'a)
      a)
    ;; test if
    (begin
      (define x 1)
      (if #t
          (set! x 10)
          (set! x 20))
      x)
    (begin
      (define y 1)
      (if #f
          (set! y 10)
          (set! y 20))
      y)
    ;; test lambda
    (begin
      (define f
        (lambda (x)
          (lambda (y)
            (+ x x y))))
      ((f 10) 20))
    ;; test currying
    (((lambda (x) (lambda (y) (- x y))) 10) 20)
    ;; test begin
    (begin
      1)
    (begin
      (+ 10 20)
      (+ 20 30))
    (begin
      (define x 10)
      (define y 20)
      (+ x x y))
    (begin
      (define x 10)
      (set! x 20)
      x)
    ;; test application
    (begin
      (define f
        (lambda (a b c d)
          (* (+ a b) (- c d))))
      (f 10 20 30 40))
    ;; test complex function application
    ((lambda (x y z)
       (+ x (* y z) z))
     (+ 10 10)
     (* 2 20)
     ((lambda (t) (* t t)) 100))
    ))


