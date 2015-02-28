(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_42_compiler.scm")

;; let verifier know that we are adding one operation
(set! primitive-operations
      (set-union primitive-operations
                 '(get-global-environment)))

(define machine-ops-builder
  (let ((liftable-prims (set-delete 'get-global-environment
                                    primitive-operations)))
    (lambda (m)
      `(
        ,@(map to-machine-prim-entry primitive-operations)
        (get-global-environment
         ,(lambda ()
            ;; TODO: install value before starting the machine
            (machine-extra-get m 'global-env 'error)))
        (error ,(lambda args
                  (apply error args)))
        ,@(default-ops-builder m)))))

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
;; insert in the back of the frame (but if you want to insert new bindings
;; in the back of the frame, you need to lookup variables from right-to-left
;; to make variable-shadowing work correctly).

;; TODO: (optional) maybe I'll try to do this backward-inserting implementation
;; and see if it works

;; However, if we allow only the top-level definitions,
;; our lexical addressing strategy will still work.
;; The important point here is that: if the program is correct,
;; but the compile time environment fails to find the variable
;; this can only mean that the variable is a top-level definition,
;; so we can just try to lookup it in the global environment
;; instead of traversing all layers of frames to go to there.

;; NOTE: in this version of the compiler,
;; non-top level definitions are not allowed.
;; i.e. you are not allowed to use "define" forms
;; inside "begin"/"lambda" or even the body of "define"
;; see the comments above for details
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
