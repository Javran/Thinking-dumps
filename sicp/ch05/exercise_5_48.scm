(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

;; based on ex 5.47

(load "exercise_5_47_eval.scm")
(load "exercise_5_47_compiler.scm")

;; TODO: an interface for REPL to compile code
;;   most importantly, we should have the ability of doing
;;   runtime assembling.

;; TOOD:
;; For now I can't see why we need an extra quote.
;; Namely we need to implement something that runs in REPL:
;;
;; (compile-and-run (quote expr))
;;
;; but if "compile-and-run" is a special form,
;; we don't have to quote the expression inside.

;; TODO: plan:
;; * implement "compile-and-run" as a special form
;; * something corresponding to "compile-and-run" as a machine primitive
;;   (this something need to have a different name, the distinction is
;;   important here, because we want to insert assembled instructions
;;   into the machine object, which means lifting the primitive from
;;   scheme directly is not going to work.)
;; * run the code after assembling, transfer control back to REPL
