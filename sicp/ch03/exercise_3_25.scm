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

(define (insertx! keys value table)
  (cond ((null? keys) (error "key list is empty"))
        ((null? (cdr keys))
          ; the last pair
          (insert! (car keys) value table))
        (else
          (let ((record (lookup (car keys) table)))
            (if record
              (insertx! (cdr keys) value record)
              (let ((subtable (make-table)))
                (insertx! (cdr keys) value subtable)
                (insert! (car keys) subtable table)))))))


(let ((test (make-tablex)))
  (insert! 'a 'val1 test)
  (insert! 'b 'val2 test)
  (insertx! '(c d e) 'val3 test)
  (insertx! '(c d g) 'val4 test)
  (out test)
  (out (lookupx '(c d g) test)))

(end-script)
