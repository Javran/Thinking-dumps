(load "../common/utils.scm")
(load "../common/defstruct.scm")
(load "../common/alist.scm")

(define my-table (make-table))

(table-put! my-table 'one 1)
(table-put! my-table 'two 2)
(table-put! my-table 'three 3)

(define out-key-value
  (lambda (k v)
    (display k)
    (display " -> ")
    (display v)
    (newline)))

(out
   (table-get my-table 'four 'default 'ignore 'ignore))
; symbol "default"

(table-for-each my-table out-key-value)
(newline)

(out
  my-table
  ; the underlying structure
  (table-get my-table 'two)
  ; 2
  (table-get my-table 'three)
  ; 3
  (table-get my-table 'four)
  ; <undefined>
  (table-get my-table 'one)
  ; 1
  )
