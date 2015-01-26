;; some extra functionalities for the compiler

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
