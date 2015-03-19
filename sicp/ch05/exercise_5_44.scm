(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_44_compiler.scm")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")
(load "exercise_5_43_tests.scm")

;; plan:
;; * tests

;; testing the compiler
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

(out
 (compile-and-run-with-env
  '(begin
     ;; the detail of matrix implementation is irrelevant
     ;; so let's just keep it symbolic.
     (define (+matrix mat1 mat2)
       (list '+matrix mat1 mat2))
     (define (*matrix mat1 mat2)
       (list '*matrix mat1 mat2))
     (define (calc + * a b x y)
       (+ (* a x) (* b y)))
     (calc +matrix *matrix 'a 'b 'x 'y))
  (init-env)))

(end-script)
