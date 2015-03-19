(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_44_compiler.scm")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")
(load "exercise_5_43_tests.scm")
(load "exercise_5_44_tests.scm")

;; thanks to our local definition-eliminating transformer,
;; our solution is actually stronger than the exercise expects:
;; as long as we don't define/set! open-coded operators
;; in the top level environment, local definitions can be eliminated
;; and compile-time environment can be used for better performance
;; and the correctness can be preserved even if we have local definitions
;; in the original expression.

;; testing the compiler
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

;; example in the book
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
