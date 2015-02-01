(define test-exps
  `(
    ;; ==== basic tests from ec-tests.scm
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
    ;; ==== tests about derived forms
    ;; test cond
    ;; - normal expressions
    (cond (else 10))
    (cond ((= 1 1) 10)
          ((= 2 1) 20))
    (cond ((= 2 1) 10)
          ((= 1 1) 20))
    (cond (#f 30)
          (else 40))
    ;; - test side effects
    (begin
      (define x 0)
      (cond (#f (set! x 10)
                20)
            (#t (set! x (+ x 5)))
            (else (set! x (+ x 100))))
      x)
    ;; re-test define
    ;; and this time both forms will be tested
    (begin
      (define x 10)
      x)
    (begin
      (define (f x y)
        (+ x x y))
      (f 10 200))
    ;; test let
    ;; - normal let-expression
    (let ((x 1)
          (y 2))
      (let ((x 10))
        ;; shadowing
        (+ x y)))
    (let ((x 1))
      (set! x 10)
      (set! x (+ x 200))
      x)
    ;; - name let
    (let loop ((i 0)
               (acc 0))
      10
      20 ;; test sequence
      (if (> i 10)
          acc
          (loop (+ i 1) (+ acc i))))
    ))
