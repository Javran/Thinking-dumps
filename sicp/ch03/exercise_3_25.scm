(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./3_3_3_representing_tables_lib.scm")

(define make-tablex make-table)
(define (lookupx keys table)
  (if (null? keys)
    table
    (let ((record (lookup (car keys) table)))
      (if record
        (lookupx (cdr keys) record)
        #f))))

(let ((test (make-tablex)))
  (insert! 'a 'val1 test)
  (insert! 'b 'val2 test)
  (out (lookupx '(a) test)))

(end-script)
