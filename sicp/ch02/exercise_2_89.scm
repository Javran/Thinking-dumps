(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./5_3_polynominal_setup.scm")
(load "./exercise_2_87_changes.scm")
(load "./exercise_2_88_changes.scm")

(load "./exercise_2_89_changes.scm")

(run-test 'poly-termlist-dense-package)

(let ((test-list (attach-tag 'poly-termlist-dense (list-in-range 1 4))))
  (out test-list
       (apply-generic 'first-term test-list)
       (apply-generic 'rest-terms test-list)))
(newline)
(let* ((make (get 'make 'poly-term))
       (x1 (make 3 (make-complex-ri 3  4)))
       (x2 (make 5 (make-complex-ri 3 -4))))
  ((compose out to-string coeff) (mul x1 x2)))

(end-script)
