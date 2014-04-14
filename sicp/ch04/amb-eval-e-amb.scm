(define amb-choices cdr)

(define (analyze-amb exp)
  (let ((cprocs (map amb-analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (install-amb-amb)

  ;; analyze-amb is defined outside

  (define (test)
    'todo)

  (define handler
    (make-amb-handler
     'amb
     analyze-amb
     test))

  (ahandler-register! handler)
  'ok)
