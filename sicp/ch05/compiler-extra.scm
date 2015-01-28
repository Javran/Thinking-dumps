;; some extra functionalities for the compiler
(load "./simu.scm")

(define (print-instruction-sequence insn-seq)
  (format #t "Registers needed: ~A~%~
              Registers modified: ~A~%~
              ;;;; Instruction listing:~%"
          (registers-needed insn-seq)
          (registers-modified insn-seq))
  (for-each (lambda (insn)
              (format #t "  ~A~%" insn))
            (statements insn-seq))
  (out ";;;; End of listing"))

;; TODO: actually we know which set of operations
;; we will be using,
;; lift them beforehands,
;; instead of lifting them on the fly

;; from "simu_ec_patch.scm"
(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

;; compile the expression
;; and run it on the machine
(define (compile-and-run exp)
  (let* ((compiled (compile exp 'val 'next))
         (insn-seq (statements compiled)))
    ;; check register requirement
    (let ((needed (registers-needed compiled)))
      (assert (or (null? needed)
                  (equal? needed '(env)))
              "the only required register (if any) should be 'env'"))
    ;; verify labels
    (if (check-labels insn-seq)
        'ok
        ;; not actually reachable
        (out "Error regarding labels occurred."))

    ;; extract required operations
    (define req-ops
      (map car (extract-operations insn-seq)))
    ;; create operation table builder
    (define (ops-builder m)
      (let* ((old-ops (default-ops-builder m))
             (missing-opnames (set-difference
                                req-ops
                                (map car old-ops)))
             ;; lift missing operations from scheme env
             (new-ops (map to-machine-prim-entry missing-opnames)))
        (append new-ops old-ops)))
    (ops-builder (empty-machine))

    ;; TODO
    ;; - execute the code
    ;; - get result
    ))

;; Local variables:
;; proc-entry: "./compiler-tests.scm"
;; End:
