(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_40_compiler.scm")
;; TODO: for now I have no idea how to deal with "define" bindings.
;; we have to come back later.

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

;; TODO: for now the test cases here are not doing much
;; except making sure things don't break
;; let's see if we want to change it when we come back...

;; TODO: tmp fix, will remove later
;; needs one more arg to compile
(define (compile-and-check exp)
  (let ((compiled (compile exp 'val 'next (empty-ctenv) )))
    (assert (check-instruction-sequence compiled)
            ;; the error message is not actually reachable
            "instruction sequence check failed.")
    compiled))

(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
