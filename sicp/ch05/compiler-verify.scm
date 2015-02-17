;; some extra functionalities for verifying the compiled code

;; just borrowing some utililties not the implementation though.
(load "simu_utils.scm")

;; by searching "(op <something>)" from compiler's source code
;; we are able to extract all possible primitives that
;; our compiled code might use.
(define primitive-operations
  '(false?
    lookup-variable-value
    set-variable-value!
    define-variable!
    make-compiled-procedure
    compiled-procedure-env
    extend-environment
    list
    cons
    compiled-procedure-entry
    primitive-procedure?
    apply-primitive-procedure))

;; check the full compiled instruction sequence
;; if there are errors, error will be raised or the return value
;; will be #f or otherwise #t.
(define (check-instruction-sequence compiled-seq)
  (let ((insn-seq (statements compiled-seq))
        (needed (registers-needed compiled-seq)))
    ;; * the compiled code should only
    ;; require "env" register being initialized if any.
    (assert (or (null? needed)
                (equal? needed '(env)))
              "the only required register (if any) should be 'env'")
    ;; * verify labels
    (if (check-labels insn-seq)
        'ok
        ;; not actually reachable
        (out "Error regarding labels occurred."))

    ;; * operations should only be taken from
    ;;   a list of allowed operations
    (let ((operations (map car (extract-operations insn-seq))))
      (assert (set-subset<=? (remove-duplicates operations)
                             primitive-operations)
              "unknown operation found"))

    #t))

;; compile the top-level expression and check the compiled code
(define (compile-and-check exp)
  (let ((compiled (compile exp 'val 'next)))
    (assert (check-instruction-sequence compiled)
            ;; the error message is not actually reachable
            "instruction sequence check failed.")
    compiled))
