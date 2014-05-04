(load "./my-eval-maybe.scm")
(load "./amb-eval-e-simple.scm")
(load "./amb-eval-ahandler.scm")
(load "./amb-eval-apply.scm")

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
   (if (application? exp)
       (just
        (analyze-application exp))
       nothing))

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

;; the amb eval procedure which looks like
;; a normal eval procedure, and returns a list of all possible solutions
(define (amb-eval-all exp env)
  (define (build-up-results result next-alternative)
    (cons result (next-alternative)))

  (define (result-list-end) '())

  (amb-eval exp env
            build-up-results result-list-end))

;; a lazy stream of the evaluation results
(define (amb-eval-stream exp env)
  (define (build-up-results-lazy
           result
           next-alternative)
    (cons-stream result (next-alternative)))

  (define (result-list-end) '())

  (amb-eval exp env
            build-up-results-lazy result-list-end))

;; take first few elements from the stream
;; and convert them into lists
(define (stream-take n xs)
  (if (<= n 0)
      '()
      (cons
       (stream-car xs)
       (stream-take (sub1 n) (stream-cdr xs)))))

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:
