(define (lookup key table)
  (let ((record (my-assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (my-assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (my-assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (my-assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value)
                            (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
