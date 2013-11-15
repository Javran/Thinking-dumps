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


(let ((tb (make-tablex)))
  ; supports arbitary length of "key path"
  ;   however, we cannot distinct a value from a table
  ;   if the length of the "key path" differs
  ;   we should never use a prefix of any key path that
  ;   has been assigned previously
  (insertx! '(a b c d e) "abcde" tb)
  (insertx! '(a b c d f) "abcdf" tb)
  (insertx! '(a b d) "abd" tb)
  (insertx! '(c d) "cd" tb)
  (insertx! '(e) "e" tb)

  (for-each
    (lambda (path)
      (out (lookupx path tb)))
    '((a b c d e)
      (a b c d f)
      (a b d)
      (c d)
      (e)
      (f a l s e))))

(end-script)
