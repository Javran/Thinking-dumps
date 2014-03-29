(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./3_3_3_representing_tables_lib.scm")

; this time I attempt to write a 2d table based on the original one
;   the idea is to make tables nested, when key-1 is given,
;   we look into a subtable, which is the value part of the outer table
;   and when key-2 is further given, we can simply lookup into the subtable,
;   this time, the value indicated by key-1 and key-2 will get fully exposed.

; masking lib impl
(define lookup-1d lookup)
(define lookup nil)

(define insert-1d! insert!)
(define insert! nil)

(define (lookup key-1 key-2 table)
  (let ((subtable
          (lookup-1d key-1 table)))
    (if subtable
      (lookup-1d key-2 subtable)
      #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable
          (lookup-1d key-1 table)))
    (if subtable
      (insert-1d! key-2 value subtable)
      (let ((subtable (make-table)))
        (insert-1d! key-2 value subtable)
        (insert-1d! key-1 subtable table)))
  'ok))

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
