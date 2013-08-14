(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./5_3_polynominal_setup.scm")
(load "./exercise_2_87_changes.scm")

(load "./exercise_2_88_changes.scm")

(let ((p1 (make-poly
            'x
            (make-termlist-from-args ; 4x^3 + 3x^2 + 2x
              3 (make-scheme-number 4)
              2 (make-scheme-number 3)
              1 (make-scheme-number 2))))
      (p2 (make-poly
            'x
            (make-termlist-from-args ; 3x^3 + 4x^2 + x + 1
              3 (make-scheme-number 3)
              2 (make-scheme-number 4)
              1 (make-scheme-number 1)
              0 (make-scheme-number 1)))))
  ; should be: x^3-x^2+x^1-1
  (out (to-string (sub p1 p2))))

(end-script)
