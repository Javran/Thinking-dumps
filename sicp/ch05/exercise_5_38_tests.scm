(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_38_compiler.scm")

;; tests and also examples
(do-test
 transform-right
 (list
  ;; a regular one
  (mat '(+ 1 2 3 4) 0
       '(+ 1 (+ 2 (+ 3 4))))
  ;; there are two operands already - keep unchanged
  (mat '(* 1 2) 1
       '(* 1 2))
  ;; single operand - remove function call
  (mat '(+ 1) 0
       1)
  ;; called with nothing - this won't usually happen
  ;; but when it happens, we return "exp-zero"
  ;; which should be the "zero" of the corresponding monoid
  (mat '(+) 0
       0)))

;; run tests on the modified compiler
(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

;; needs few more to test open-code primitives
(define test-exps-ex-5-38
  `(;; complex & mixed
    (+ (* 1 2 3) (+ 1 (- 20 19 19) (- 18 20) 3 (* 4 5))
       (+ 6 (+ 1 2 4) 8))
    ;; simple
    (+)
    (*)
    (+ 1)
    (* 10)
    ;; mixed
    (= (+ 1 2 3) (* 1 2 3))
    ))
(set! test-exps
      (append test-exps test-exps-ex-5-38))

(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
