(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(define test-code
  ;; if arguments are evaluated from left to right:
  ;; * x = 10
  ;; * x = x + 2 = 12, context: (+ _ ?)
  ;; * x = x * 10 = 120, context: (+ 12 _)
  ;; * (+ 12 120) = 132
  ;; if arguments are evaluated from right to left:
  ;; * x = 10
  ;; * x = x * 10 = 100, context: (+ ? _)
  ;; * x = x + 2 = 102, context: (+ _ 100)
  ;; * (+ 100 102) = 202
  '(begin
     (define x 10)
     (+ (begin
          (set! x (+ x 2))
          x)
        (begin
          (set! x (* x 10))
          x))))

;; the compiler evaluates argument list from right to left,
;; should produce 202
(out (compile-and-run test-code))

(load "exercise_5_36_compiler.scm")
;; rerun tests
(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

;; after applying the patch, the compiler evaluates argument list
;; from left to right, should produce 132 this time
(out (compile-and-run test-code))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
