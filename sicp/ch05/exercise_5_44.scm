(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_44_compiler.scm")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")
(load "exercise_5_43_tests.scm")

(compile-and-run-with-env
 '(begin
    (define x 10)
    x
    (begin
      (define y (if #t 20 40))
      (set! y 30))
    (define (f u v) (* u v))
    (define t (lambda (x) x))
    (+ (t x) (cond (#f (t y))
                   (else (t (t y))))
       (let ()
         (define k (+ 2 x))
         (+ k (f x x)))))
 (init-env))

;; testing the compiler
#;
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)
