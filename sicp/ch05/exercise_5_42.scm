(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_42_compiler.scm")

;; it's tricky to implement this operation
;; as it depends on the machine - each machine
;; has its own instance of global-environment
;; we work around it by pretending "get-global-environment"
;; to be a primitive operation (to make it work with the compile-and-check)
;; but binding the operation handler to do the real job
(define get-global-environment
  "this variable should never be used")

(set! primitive-operations
      (set-union primitive-operations
                 '(get-global-environment)))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      (let* ((old-ops (old-builder m)))
        `((get-global-environment
           ,(lambda (m)
              (machine-extra-get m 'global-env 'error)))
          ;; the inner "get-global-environment"
          ;; should be shadowed now
          ,@old-ops)))))

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
