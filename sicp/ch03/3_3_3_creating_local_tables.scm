(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record
                  (assoc key-2 (cdr subtable))))
            (if record (cdr record) #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record
                  (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (lookup k1 k2 d)
  ((d 'lookup-proc) k1 k2))

(define (insert! k1 k2 v d)
  ((d 'insert-proc!) k1 k2 v))

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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(end-script)
