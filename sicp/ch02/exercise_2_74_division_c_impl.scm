(define (install-division-c-impl)
  (define (get-record name data)
    (let* ((employee-data-filtered
             (filter (lambda (entity)
                       (equal? (car entity) name))
                     data))
           (employee-data
             (map cdr employee-data-filtered)))
      (if (null? employee-data)
        #f
        (cons name employee-data))))

  (put 'get-record 'division-c get-record)
  )

; install automatically
(install-division-c-impl)
