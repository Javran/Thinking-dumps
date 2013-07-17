(define (install-division-b-impl)
  (define get-record
    (association-procedure
      ; use `equal?` as predicate
      equal?
      ; select the value of `name` from dict
      (lambda (record)
        (cadr (assoc 'name record)))))
  (define (get-salary record)
    (cadr (assoc 'salary record)))

  (put 'get-record 'division-b get-record)
  (put 'get-salary 'division-b get-salary)
  )

; install automatically
(install-division-b-impl)
