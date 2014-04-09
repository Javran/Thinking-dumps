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

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:
