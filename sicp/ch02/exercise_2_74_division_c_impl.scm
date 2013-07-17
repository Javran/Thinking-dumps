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
  (define (get-salary record)
    (cadr (assoc 'salary (cdr record))))

  (put 'get-record 'division-c get-record)
  (put 'get-salary 'division-c get-salary)
  )

; install automatically
(install-division-c-impl)
