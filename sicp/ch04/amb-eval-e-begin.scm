(define (analyze-sequence exps)
  (define (sequentially a b)
    ;; evaluate a, then b
    (lambda (env succeed fail)
      (a env
         ;; success cont for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure cont for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))
