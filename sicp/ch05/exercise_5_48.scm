(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

;; NOTE:
;; turns out it is not necessary to quote the expression
;; if we add "compile" as a machine operation.
;;
;; In the book, it seems to suggest installing
;; "compile-and-run" as a primitive procedure so that calling the compiler
;; could be a function application.
;; However, this approach did not work out for me because
;; primitive procedure in implemented language can only produce
;; a value, but does not have the direct ability to control the machine
;; (however, in theory this is possible, since the machine object is there)

;; here is my approach:
;; * a special form "(compile-and-run (quote <exp))"
;;   is added. ("exercise_5_48_eval.scm" for evaluator support,
;;   "exercise_5_48_prim.scm" for expression predicate and accessor)
;; * new label "ev-compile-and-run" for handling:
;;   - it extract the expression, and call "magic-compile"
;;     for compilation.
;;   - the compiled instructions get assembled
;;     and the entry point for evaluating this expression
;;     get written to "val" register
;;   - we proceed to call "val" register,
;;     and "continue" register will lead us back
;;     to complete the REPL loop.
;; * as mentioned before new primitive machine operation
;;   "magic-compile" is inserted in "exercise_5_48_machine.scm",
;;   it compiles the code using "return" as linkage
;;   (so that after the call "continue" register can
;;   be directly lead us back)

;; based on ex 5.47
(load "exercise_5_47_compiler.scm")

(load "exercise_5_48_eval.scm")
(load "exercise_5_48_prim.scm")
(load "exercise_5_48_machine.scm")

;; load this module to play with it dirrectly
(compile-and-go ''())
