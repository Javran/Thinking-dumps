(define (install-division-a-impl)
  (define get-record assoc)
  (define (get-salary record)
    (cdr (assoc 'salary (cadr record))))

  (put 'get-record 'division-a get-record)
  (put 'get-salary 'division-a get-salary)
  )

; install automatically
(install-division-a-impl)
