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
      (x3 (make-termlist-from-args
            ; x^3 + x
            3 (make-scheme-number 1)
            1 (make-scheme-number 1)))
      (x4 (make-termlist-from-args
            ; x - 1
            1 (make-scheme-number 1)
            0 (make-scheme-number -1))))
  (out (sub (sub x1 (mul x2 x3)) x4)))

(run-test 'poly-termlist-package-2)

(end-script)
