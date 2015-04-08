(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

;; based on ex 5.47

(load "exercise_5_48_eval.scm")
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

;; TODO: plan: (pause this plan)
;; * implement "compile-and-run" as a special form
;; * something corresponding to "compile-and-run" as a machine primitive
;;   (this something need to have a different name, the distinction is
;;   important here, because we want to insert assembled instructions
;;   into the machine object, which means lifting the primitive from
;;   scheme directly is not going to work.)
;; * run the code after assembling, transfer control back to REPL

;; TODO: now I guess the exercise want us to implement
;; "compile-and-run" as a function available at run time.
;; and then the quotation makes sense. so there are many ways of doing things,
;; let's first take the way our exercise wants and then think about other approaches.

;; extend the environment to
;; install machine-related primitives (currently compile-and-run only)
(define (install-machine-primitives env machine)
  (define (compile-and-run-prim exp)
    (let ((compiled
           (compile exp 'val 'return))
          (insn-seq (statements compiled)))
      (assemble insn-seq machine)
      ;; at this point .. well I realize
      ;; we don't know how to transfer the control
      ;; decently. -- yes it's doable by manipulate the machine
      ;; but then the implementation would look weird.
      ))
  (extend-environment
   '(compile-and-run)
   (lift-primitive compile-and-run-prim)
   env))
