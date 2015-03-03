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
;; see the comments above for details.
;; however, since "begin" form turns out not to create
;; a new layer of frame, definitions inside a top-level
;; "begin" form are also okay.
(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

;; NOTE: it turns out that "begin" form does not create new layer of frame,
;; try the followiing s-exp out:
;;
;; > (define x 10)
;; > (begin (define x 1))
;; > x
;; 1
;;
;; in most implementations the final expression
;; produces 1 rather than 10.
;; therefore we can say that evaluating top-level "begin"-forms are just like
;; evaluating its subexpressions in top-level form.
;; the invariant that if we cannot find a variable in compile-time environment
;; it must be in the top-level runtime environment.

(for-each
 (test-evaluator
  compile-and-run-with-env)
 (delete
  ;; name-let is not supported
  ;; because the transformation uses a inner "define"-form
  `(let loop ((i 0)
              (acc 0))
     10
     20 ;; test sequence
     (if (> i 10)
         acc
         (loop (+ i 1) (+ acc i))))
  test-exps))

;; TODO: try nested-lambda as the exercise suggests

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
