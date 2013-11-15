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

; insert and return the value
(define (insertx! keys value table)
  (if (null? keys)
    value
    (let ((record (lookup (car keys) table)))
      (if record
        (let ((return (insertx! (cdr keys) value record)))
          (insert! (car keys) return record))
        (let* ((subtable (make-table))
               (return (insertx! (cdr keys) value subtable)))
          (insert! (car keys) return subtable))))))


(let ((test (make-tablex)))
  (insert! 'a 'val1 test)
  (insert! 'b 'val2 test)
  (out (lookupx '(a) test))
  (insertx! '(a b c) 'val3 test)
  (out test))

(end-script)
