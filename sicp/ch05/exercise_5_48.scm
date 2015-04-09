(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

;; based on ex 5.47
(load "exercise_5_47_compiler.scm")

(load "exercise_5_48_eval.scm")
(load "exercise_5_48_prim.scm")
(load "exercise_5_48_machine.scm")

;; (compile-and-go ''())

(assert
 (equal?
  (compile-then-interp-with-env
   ;; leave nothing to compile
   ''()
   ;; the expression below is interpreted
   ;; at runtime.
   `(begin
      (define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n)))
      (factorial 5))
   (init-env))
  ;; 5! = 120
  120))

;; TOOD:
;; For now I can't see why we need an extra quote.
;; Namely we need to implement something that runs in REPL:
;;
;; (compile-and-run (quote expr))
;;
;; but if "compile-and-run" is a special form,
;; we don't have to quote the expression inside.

;; TODO:
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

;; NOTE: I find it's difficult to implement "compile" as a
;; primitive procedure, because after compilation,
;; we still need to transfer control to call the newly generated code
;; which can be tricky to make it right by allowing a primitive procedure
;; to manipulate machine states. let's go back and try the special-form method
