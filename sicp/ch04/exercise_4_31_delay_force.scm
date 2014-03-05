(load "./exercise_4_31_promise.scm")

;; support for `delay` and `force`

(define (install-eval-delay)

  (define (eval-delay exp env)
    (make-promise exp env))

  ;; some sort of `native` way
  ;; instead of using `promise`
  (define (analyze-delay exp)
    (let ((result #f)
          (analyzed (my-analyze exp)))
      (lambda (env)
        (if result
            (cdr result)
            (let ((current-result
                   (analyzed env)))
              (set! result (cons 'done current-result))
              current-result)))))

  (define (test-eval eval-delay)
    ;; here we avoid using any knowledge of `promise`
    ;; thus need `force` to be inserted when testing
    ;; this special form.

    ;; correctness test
    (define env (init-env))
    (do-test
     eval-delay
     (list
      (mat `(+ 1 2 3 4) env 10)
      (mat `'symbol env 'symbol)
      (mat `"test" env "test")
      ))

    ;; more tests pending
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
  'ok)

(define (install-eval-force)
  'todo)

;; Local variables:
;; proc-entry: "./exercise_4_31.scm"
;; End:
