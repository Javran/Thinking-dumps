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
  (define (insertx-intern! keys value table)
    (if (null? keys)
      value
      (let ((record (lookup (car keys) table)))
        (if record
          (let ((return (insertx-intern! (cdr keys) value record)))
            (out return)
            (insert! (car keys) return table)
            record)
          (let* ((subtable (make-table))
                 (return (insertx-intern! (cdr keys) value subtable)))
            (insert! (car keys) return subtable)
            subtable)))))
  (insert! (car keys) (insertx-intern! keys value table) table)
  table)


(let ((test (make-tablex)))
  (insert! 'a 'val1 test)
  (insert! 'b 'val2 test)
  (insertx! '(c d a) 'val3 test)
  (out (lookupx '(c d a) test))
  (out test))

(end-script)
