(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

;; based on ex 5.47

(load "exercise_5_48_eval.scm")
(load "exercise_5_47_compiler.scm")

(define compile-and-run?
  (list-tagged-with 'compile-and-run))

(define (compile-and-run-exp obj)
  ;; one cadr for destructing "compile-and-run"
  ;; one cadr for destructing "quote"
  (cadr (cadr obj)))

(define (ec-ops-builder-modifier current-ops-builder)
  (lambda (m)
    (let* ((old-ops
            ;; step #1: passing the machine to generate
            ;; a list of operations
            (current-ops-builder m))
           (current-ops
            ;; step #2: adding some more (these primitives
            ;; usually need to know the machine object,
            ;; thus cannot be lifted from scheme)
            `((get-global-environment
               ,(lambda ()
                  (machine-extra-get m 'global-env 'error)))
              (error ,(lambda args
                        (apply error args)))
              ;; let's call this new primitive operation
              ;; "magic-compile" -- why not :)
              (magic-compile
               ,(lambda (exp)
                  (let* ((compiled (compile exp 'val 'return))
                         (insn-seq (statements compiled))
                         (entry (assemble insn-seq m)))
                    ;; put instruction sequence in "val"
                    ;; and we are done
                    entry)))
              ,@old-ops))
           (missing-prim-symbols
            ;; step #3: find the list of missing operations
            ;; and lift them from scheme
            (set-diff
             (ec-get-required-operations)
             (remove-duplicates
              (map car current-ops)))))
      `(
        ,@(map to-machine-prim-entry missing-prim-symbols)
        ,@current-ops))))

(compile-and-go ''())

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

;; NOTE: I find it's difficult to implement "compile" as a
;; primitive procedure, because after compilation,
;; we still need to transfer control to call the newly generated code
;; which can be tricky to make it right by allowing a primitive procedure
;; to manipulate machine states. let's go back and try the special-form method


;; extend the environment to
;; install machine-related primitives (currently compile-and-run only)
#;(define (install-machine-primitives env machine)
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
