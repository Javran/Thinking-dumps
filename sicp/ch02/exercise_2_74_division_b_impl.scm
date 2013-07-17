(define (install-division-b-impl)
  (define get-record
    (association-procedure
      ; use `equal?` as predicate
      equal?
      ; select the value of `name` from dict
      (lambda (record)
        (cadr (assoc 'name record)))))

  (put 'get-record 'division-b get-record)
  )

; install automatically
(install-division-b-impl)
