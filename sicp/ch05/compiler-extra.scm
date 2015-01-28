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

;; compile the expression
;; and run it on the machine
(define (compile-and-run exp)
  (let* ((compiled (compile exp 'val 'next))
         (insn-seq (statements compiled)))
    ;; verify labels
    (if (check-labels insn-seq)
        (out "Labels are checked.")
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
                                (map car old-ops))))
        (out "Missing operations:")
        (for-each
         (lambda (x)
           (format #t "* ~A~%" x))
         missing-opnames)
        old-ops))
    (ops-builder (empty-machine))

    ;; TODO
    ;; - lift from scheme if necessary
    ;; - execute the code
    ;; - get result
    ))

;; Local variables:
;; proc-entry: "./compiler-tests.scm"
;; End:

