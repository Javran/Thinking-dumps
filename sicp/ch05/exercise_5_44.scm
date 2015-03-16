(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_44_compiler.scm")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")
(load "exercise_5_43_tests.scm")

;; the transformation is consistent if
;; the results of executing both the original expression
;; and the transformed one are always the same
(define (check-transform-consistency exp)
  (let* ((new-exp (transform-exp exp)))
    (equal? (eval exp user-initial-environment)
            (eval new-exp user-initial-environment))))

(assert
 (andf
  (map
   check-transform-consistency
   test-exps)))

;; testing the compiler
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)
