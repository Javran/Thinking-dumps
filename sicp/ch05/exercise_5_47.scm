(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "exercise_5_47_eval.scm")
(load "exercise_5_47_compiler.scm")

(load "ec-tests.scm")
(for-each
 (test-evaluator machine-eval)
 test-exps) (newline)

;; some testcases might fail when the compilation
;; is not fully working, this is desired because
;; at compile time we cannot check the object and
;; make the final decision so we must take all the cases
;; into account, the compilation of compound procedure application
;; is one of these cases.
(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps) (newline)

;; TODO: need to summarize 3 cases.
;; TODO: console-free testcases?
(compile-and-go
 '(begin
    ;; for triggering target == val && linkage == return
    ;; use: (test-apply (lambda (x) x))
    (define (test-apply f)
      (f 10))
    ;; for triggering target == val && linkage /= return
    ;; use: (test-apply2 (lambda (x) x))
    ;; some extra stuff after function application
    ;; so that the linkage is not "return"
    (define (test-apply2 f)
      (+ (f 10) 20))

    ;; for triggering target /= val && linkage /= return
    ;; use: (test-apply3 (lambda (x) x))
    ;; "(f identity)" has to be compound procedure if "f"
    ;; is given in the interpreter, therefore the target of
    ;; compiling "(f identity)" goes to "proc" /= "val"
    ;; as we need to apply arguments after it, the linkage
    ;; cannot be "return"
    (define (identity x) x)
    (define (test-apply3 f)
      ((f identity) 10))
    ))
