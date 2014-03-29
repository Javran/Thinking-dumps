; handle quotations

(define quoted?
  (list-tagged-with 'quote))

(define (install-eval-quote)

  (define text-of-quotation cadr)

  (define (eval-quote exp env)
    (text-of-quotation exp))

  (define (analyze-quote exp)
    ;; the `exp` should be a constant
    ;; so we call `text-of-quotation` before
    ;; the evaluation
    (let ((result (text-of-quotation exp)))
      (const result)))

  (define (test-eval eval-quote)
    (let ((testcases
           (list
            (mat '(quote a) #f 'a)
            (mat '(quote "a") #f "a")
            (mat '(quote 1) #f 1))))
      (do-test eval-quote testcases))
    'ok)

  (define handler
    (make-handler
      'quote
      eval-quote
      analyze-quote
      (test-both
       test-eval
       eval-quote
       analyze-quote)))

  (handler-register! handler)
  'ok)

;; Local variables:
;; proc-entry: "./my-eval.scm"
;; End:
