(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./5_3_polynominal_setup.scm")
(load "./exercise_2_87_changes.scm")
(load "./exercise_2_88_changes.scm")
(load "./exercise_2_89_changes.scm")
(load "./exercise_2_90_changes.scm")

(load "./exercise_2_91_changes.scm")

(let ((x1 (make-termlist-from-args
            ; x^5 - 1
            5 (make-scheme-number 1)
            0 (make-scheme-number -1)))
      (x2 (make-termlist-from-args
            ; x^2 - 1
            2 (make-scheme-number 1)
            0 (make-scheme-number -1)))
      )
  (let ((p1 (make-poly 'x x1))
        (p2 (make-poly 'x x2)))
    (let* ((result (div p1 p2))
           (q (car result))
           (r (cadr result)))
      (out "divisor:" (to-string p1)
           "dividend:" (to-string p2)
           "quotient:" (to-string q)
           "remainder:" (to-string r)))))

(run-test 'poly-termlist-package-2)

(end-script)
