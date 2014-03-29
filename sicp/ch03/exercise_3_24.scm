(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (make-table . same-key?)
  (let ((sk? (if (null? same-key?)
               equal?
               (car same-key?)))
        (table (list '*table*)))
    (define (lookup k)
      ; please refer to:
      ; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Association-Lists.html
      (let ((result
              ((association-procedure sk? car) k (cdr table))))
        (if result
          (cdr result)
          #f)))
    (define (insert! k v)
      (let ((record (lookup k)))
        (if record
          (set-cdr! record v)
          (set-cdr! table
                    (cons (cons k v)
                          (cdr table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)))
    dispatch))

(define (lookup k t)
  ((t 'lookup) k))

(define (insert! k v t)
  ((t 'insert!) k v))

(let ((table (make-table)))
  (let loop ((i 0))
    (if (< i 8)
      (begin
        (insert! i (+ i i) table)
        (loop (+ i 1)))))
  (assert
    (equal?
      (map
        ((curry2 (flip lookup)) table)
        (list-in-range 0 10))
      '(0 2 4 6 8 10 12 14 #f #f #f)))
  (insert! 10 'a table)
  (assert
    (equal?
      (map
        ((curry2 (flip lookup)) table)
        (list-in-range 0 10))
      '(0 2 4 6 8 10 12 14 #f #f a)))
  )

; case-insensitive key
(define (same-key? a b)
  (string=? (string-upcase a)
            (string-upcase b)))

(let ((tb (make-table same-key?)))
  (for-each
    (lambda (num)
      (let ((key (string-append "key-" (number->string num)))
            (val (* num num)))
        (insert! key val tb)))
    (list-in-range 1 10))
  (out (lookup "KEY-1" tb))
  ; 1
  (out (lookup "KeY-10" tb))
  ; 100
  (out (lookup "keY-7" tb))
  ; 49
  )

(end-script)
