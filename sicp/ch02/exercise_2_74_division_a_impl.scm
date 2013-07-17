(define (install-division-a-impl)
  (define get-record assoc)

  (put 'get-record 'division-a get-record)
  )

; install automatically
(install-division-a-impl)
