(define (analyze-if exp)
  ;; the code in book has extra parentheses,
  (let ((pproc (if-predicate exp))
        (cproc (if-consequent exp))
        (aproc (if-alternative exp)))
    (lambda (env succeed fail)
      (pproc env
             ;; do evluation (on analyzed data)
             ;; obtain pred-value.
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))
