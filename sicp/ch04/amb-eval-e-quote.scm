(load "./my-eval-e-quote.scm")
(load "./amb-eval-test.scm")

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (install-amb-quote)

  ;; analyze-quote (a.k.a analyze-quoted)
  ;; is defined outside
  (define analyze-quote analyze-quoted)

  (define (test)
    (let ((env (init-env)))
      (do-test
       test-eval
       (list
        (mat `(quote (a b c)) env `(a b c))
        (mat `(quote (+ 1 2)) env `(+ 1 2))
        )
       (test-compare equal?))
      'ok))

  (define handler
    (make-amb-handler
     'quote
     analyze-quote
     test))

  (ahandler-register! handler)
  'ok)
