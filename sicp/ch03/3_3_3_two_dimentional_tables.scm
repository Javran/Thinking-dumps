(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (lookup key-1 key-2 table)
  (let ((subtable
          (assoc key-1 (cdr table))))
    (if subtable
      (let ((record
              (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key-1
                            (cons key-2 value))
                      (cdr table)))))
  'ok)

(define (make-table)
  (list '*2d-table*))

(let ((t (make-table)))
  (insert! 'math '+ 43 t)
  (insert! 'math '- 45 t)
  (insert! 'math '* 42 t)
  (insert! 'letters 'a 97 t)
  (insert! 'letters 'b 98 t)

  (let ((testcases
          (list (mat 'math '+ t    43)
                (mat 'letters 'b t 98)
                (mat 'letters 'c t #f)
                (mat 'math '* t    42)
                (mat 'aaa 'bbb t   #f)
                )))
    (do-test lookup testcases))
  )

(end-script)
