; make sure 'defstruct' is loaded before loading this file

(defstruct table (equ eqv?) (alist '()))

; find 'k in k-v list 'al' using predicate 'equ?'
(define find-in-table
  (lambda (k al equ?)
    (let loop ((al al))
      (if (null? al) #f
          (let ((first-pair (car al)))
            (if (equ? (car first-pair) k) first-pair
                (loop (cdr al))))))))

; get value from the table, or default value 'd' is returned
(define table-get
  (lambda (tbl k . d)
    ; try to find the k-v pair in list
    (let ((c (find-in-table k (table.alist tbl) (table.equ tbl))))
      (cond (c (cdr c))
            ((pair? d) (car d))))))

; put key-value pair into a dict
(define table-put!
  (lambda (tbl k v)
    (let ((al (table.alist tbl)))
      (let ((c (find-in-table k al (table.equ tbl))))
        (if c (set-cdr! c v)
            (set!table.alist tbl (cons (cons k v) al)))))))

; call 'p' for each k-v pair in a dict
(define table-for-each
  (lambda (tbl p)
    (for-each
     (lambda (c)
       (p (car c) (cdr c)))
     (table.alist tbl))))
