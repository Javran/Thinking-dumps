(load "./my-eval-maybe.scm")
(load "./amb-eval-e-simple.scm")
(load "./amb-eval-ahandler.scm")

;; the amb analyze procedure
(define (amb-analyze exp)
  ;; try to analyze self-evaluating exp / var exp
  (define (try-simple-analyze exp)
    (cond ((self-evaluating? exp)
           (just
            (analyze-self-evaluating exp)))
           ((variable? exp)
            (just
             (analyze-variable exp)))
           (else nothing)))

  ;; try to dispatch the s-exp according to slots
  (define (try-dispatch-analyze exp)
    (if (non-empty? exp)
        ;; try to fetch the handler
        (let ((handler (my-eval-get (car exp))))
          (if handler
              (just
               (ahandler-analyze handler exp))
              nothing))
        nothing))

  ;; try to analyze an application
 (define (try-app-analyze exp)
   'todo)

 ((maybe
    ;; if the analysis goes well, return the result
    identity
    ; else there must be some errors
    (lambda ()
      (error "unknown expression:" exp)))
   (or (try-simple-analyze exp)
       (try-dispatch-analyze exp)
       (try-app-analyze exp)
       nothing)))

;; the amb eval procedure
(define (amb-eval exp env succeed fail)
  ((amb-analyze exp) env succeed fail))
