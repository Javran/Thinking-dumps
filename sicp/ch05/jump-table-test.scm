(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "jump-table.scm")

(assert
 ;; I guess one working example is enough.
 ;; unless there are bugs exposed by other usages.
 (equal?
  (build-jump-table
   '(a
     (a b)
     b
     c
     d
     (c d e)
     (e)
     (f)
     g
     (h)
     (i j k)
     l
     (j k)
     m
     (m n)
     i)
   symbol?)
  (reverse
   '( (a  ((a b) (c d e) (e) (f) (h) (i j k) (j k) (m n)))
      (b  (      (c d e) (e) (f) (h) (i j k) (j k) (m n)))
      (c  (      (c d e) (e) (f) (h) (i j k) (j k) (m n)))
      (d  (      (c d e) (e) (f) (h) (i j k) (j k) (m n)))
      (g  (                      (h) (i j k) (j k) (m n)))
      (l  (                                  (j k) (m n)))
      (m  (                                        (m n)))
      (i  (                                             ))
      ))))

(end-script)
