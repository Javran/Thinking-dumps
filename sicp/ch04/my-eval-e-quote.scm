; handle quotations

(define (install-eval-quote)

  (define text-of-quotation cadr)

  (define (eval-quote exp env)
    (text-of-quotation exp))

  (define (test)
    (let ((testcases
            (list
              (mat '(quote a) #f 'a)
              (mat '(quote "a") #f "a")
              (mat '(quote 1) #f 1))))
      (do-test eval-quote testcases equal?))
    'ok)

  (define handler
    (make-handler
      'quote
      eval-quote
      test))

  (handler-register! handler)
  'done)
