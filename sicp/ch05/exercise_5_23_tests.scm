;; adding extra test examples into test-exps
;; please make sure "test-exps" from ec-tests.scm is loaded before
;; loading this module
;; also it's recommended to load this module only once.

(define test-exps-ex-5-23
  `(
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

(set! test-exps
      (append test-exps test-exps-ex-5-23))
