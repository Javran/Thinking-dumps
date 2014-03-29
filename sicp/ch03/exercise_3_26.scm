(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_3_26_bintable.scm")

(define n (make-table))

(insert! 4 'a n)
(insert! 2 'b n)
(insert! 6 'c n)
(insert! 1 'd n)
(insert! 3 'e n)
(insert! 5 'f n)
(insert! 7 'g n)

(out (table->string n))
; dump table to string and then output

(out (map ((curry2 (flip lookup)) n)
          (list-in-range 1 7)))
; d b e a f c g

(end-script)
