(load "./amb-eval-test.scm")

(define (analyze-sequence exps)
  (define (sequentially a b)
    ;; run a, then b
    (lambda (env succeed fail)
      (a env
         ;; on success, control goes to this function
         (lambda (a-value fail2)
           ;; run b
           (b env succeed fail2))
         ;; on failure, control goes to "fail" to cleanup
         fail)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))

  (let ((procs (map amb-analyze exps)))
    (if (null? procs)
        ;; sequence cannot be empty
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (install-amb-begin)

  (define analyze-begin
    ;; remove `begin`, and the rest of the expression
    ;; should be an non-empty sequence of s-exp.
    (compose analyze-sequence cdr))

  (define (test)
    (let ((env (init-env)))
    (do-test
     test-eval
     (list
      ;; the last result is returned
      (mat `(begin 1 2 3) env 3)
      ;; handle "singleton" properly
      (mat `(begin #t) env #t)
      ;; embeded expression test
      (mat `(begin 1 2 (begin 3 4 (begin 5 6))) env 6)
      )
     (test-compare equal?)))
    'ok)

  (define handler
    (make-amb-handler
     'begin
     analyze-begin
     test))

  (ahandler-register! handler)
  'ok)

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:
