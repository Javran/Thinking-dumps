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

;; compile the expression
;; and run it on the machine
(define (compile-and-run exp)
  (let* ((compiled (compile exp 'val 'next))
         (insn-seq (statements compiled)))
    (if (check-labels insn-seq)
        (out "Labels are checked.")
        ;; not actually reachable
        (out "Error regarding labels occurred."))
    (define ops
      (map car (extract-operations insn-seq)))
    (out "Required operations:")
    (for-each
     (lambda (x)
       (format #t "* ~A~%" x))
     ops)
    ;; TODO
    ;; - lift from scheme if necessary
    ;; - execute the code
    ;; - get result
    ))

;; Local variables:
;; proc-entry: "./compiler-tests.scm"
;; End:

