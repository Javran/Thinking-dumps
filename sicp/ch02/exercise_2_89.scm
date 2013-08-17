(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./5_3_polynominal_setup.scm")
(load "./exercise_2_87_changes.scm")
(load "./exercise_2_88_changes.scm")

(load "./exercise_2_89_changes.scm")

(run-test 'poly-termlist-dense-package)

; show comparison of the underlying structures
(out "empty-termlist:"
     (the-empty-term-list)
     (the-empty-term-list-dense))
(newline)

(let ((t1-args (list 
                 ; 1/2 x^3 + 0.4 x^2 + 1
                 3 (make-rational 1 2)
                 2 (make-scheme-number 0.4)
                 0 (make-scheme-number 1)))
      (t2-args (list 
                 ; (5+3i) x^5 + (-6/7) x
                 5 (make-complex-ri 5 3)
                 1 (make-rational -6 7))))
  (let ((p1  (make-poly 'x
                        (apply make-termlist-from-args t1-args)))
        (p1d (make-poly 'x
                        (apply make-termlist-from-args-dense t1-args)))
        (p2  (make-poly 'x
                        (apply make-termlist-from-args t2-args)))
        (p2d (make-poly 'x
                        (apply make-termlist-from-args-dense t2-args))))
    (out "structure dump:" p1 p1d p2 p2d)
    (out "to-string:")
    (for-each (compose out to-string) (list p1 p1d
                                            p2 p2d))
    (out "add:")
    (for-each (compose out to-string) (list (add p1  p2 )
                                            (add p1d p2d)))
    (out "mul:")
    (for-each (compose out to-string) (list (mul p1  p2 )
                                            (mul p1d p2d)))
    ))

(end-script)
