(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_42_compiler.scm")

;; NOTE: for the idea of lexical addressing
;; to be applicable here, we assume there is no "non-top-level"
;; definitions
;; This means the following expression will not work under
;; our new evaluator:
;; ((lambda (x y)
;;    (define z 17)
;;    (define t 23)
;;    (- z x t y))
;;  11
;;  100)
;; the reason is because that definitions will call "add-binding-to-frame"
;; function to manipulate the frame. the binding is added
;; in front of the existing bindings of that frame,
;; which makes the runtime environment and compile-time environment
;; out-of-sync. A potential fix would be to make "add-binding-to-frame"
;; insert in the back of the frame.

;; TODO: see if it is applicable to apply "get-global-environment"

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
