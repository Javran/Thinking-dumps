(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./3_3_3_representing_tables_lib.scm")

(let ((t (cons
           '*table* 
           '((a . 1) (b . 2) (c . 3)))))

  (out
    ; lookup elements corresponding to
    ;   'a, 'b, 'c, 'd in t
    (map
      ((curry2 (flip lookup)) t)
      '(a b c d)))
  ; (1 2 3 #f)
  )

(let ((table (make-table)))
  (let loop ((i 0))
    (if (< i 8)
      (begin
        (insert! i (+ i i) table)
        (loop (+ i 1)))))
  (out
    (map
      ((curry2 (flip lookup)) table)
      (list-in-range 0 10)))
  ; for i = 0..7 , return i * 2
  ; for i = 8..  , return #f
  )

(end-script)
