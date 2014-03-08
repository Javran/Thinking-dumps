(load "./exercise_4_31_promise.scm")

;; support for `delay` and `force`

(define delayed-exp cadr)
(define forcing-exp cadr)

(define (install-eval-delay)

  (define (eval-delay exp env)
    (make-promise (delayed-exp exp) env))

  ;; some sort of `native` way
  ;; instead of using `promise`
  (define (analyze-delay exp)
    (let ((analyzed (my-analyze (delayed-exp exp))))
      (lambda (env)
        ;; now construct the promise

        ;; TODO:
        ;; here we lost the ability of analyzing inner structure
        ;; we should have a better way to handler this issue
        (make-promise (delayed-exp exp) env))))

  (define (test-eval eval-delay)
    ;; here we avoid using any knowledge of `promise`
    ;; thus need `force` to be inserted when testing
    ;; this special form.

    (define (test-delay exp env)
      (let ((env-extended
            (extend-environment
             '(delayed-value) ; pickup an varible name (carefully)
             (list (eval-delay `(delay ,exp) env))
             env)))
        (my-eval `(force delayed-value) env-extended)))

    ;; correctness test
    (define env (init-env))
    (do-test
     test-delay
     (list
      (mat `(+ 1 2 3 4) env 10)
      (mat `'symbol env 'symbol)
      (mat `"test" env "test")
      ))

    ;; laziness tests
    (my-eval `(define count 0) env)
    (define p (eval-delay `(delay
                             (begin
                               (set! count (+ count 1))
                               10))
                          env))
    (assert (= (lookup-variable-value 'count env) 0))

    (define new-env
      (extend-environment
       '(delayed-value)
       (list p)
       env))

    ;; side effect happens exactly once.
    (assert (= (my-eval `(force delayed-value) new-env) 10))
    (assert (= (lookup-variable-value 'count env) 1))
    (assert (= (my-eval `(force delayed-value) new-env) 10))
    (assert (= (lookup-variable-value 'count env) 1))
    (assert (= (my-eval `(force delayed-value) new-env) 10))
    (assert (= (lookup-variable-value 'count env) 1))

    'ok)

  (define handler
    (make-handler
     'delay
     eval-delay
     analyze-delay
     (test-both
      test-eval
      eval-delay
      analyze-delay)))

  (handler-register! handler)
  'ok)

(define (install-eval-force)

  (define (eval-force exp env)
    (let ((promise (my-eval (forcing-exp exp) env)))
      (promise-force promise)))

  (define (analyze-force exp)
    (let ((analyzed (my-analyze (forcing-exp exp))))
      (lambda (env)
        (let ((promise (analyzed env)))
          (promise-force promise)))))

  (define (test-eval eval-force)
    ;; `delay` and `force` exist in pair,
    ;; most of the tests should be in `delay` handler.
    ;; here is just few tests to tell if `force` works

    (define testcases
      (list
       (list 'p1 `(+ 1 2 3 4) 10)
       (list 'p2 `"test" "test")
       (list 'p3 `(- 10 8) 2)))

    (define env (init-env))

    (define delayed-values
      (map
       (lambda (exp)
         (my-eval `(delay ,exp) env))
       (map cadr testcases)))

    (define env-extended
      (extend-environment
       (map car testcases)
       delayed-values
       env))

    (do-test
     eval-force
     (map
      (lambda (testcase)
        (mat `(force ,(car testcase))
             env-extended
             (caddr testcase)))
      testcases))

    'ok)

  (define handler
    (make-handler
     'force
     eval-force
     analyze-force
     (test-both
      test-eval
      eval-force
      analyze-force)))

  (handler-register! handler)
  'ok)

;; Local variables:
;; proc-entry: "./exercise_4_31.scm"
;; End:
