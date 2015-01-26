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


;; compile the expression
;; and run it on the machine
(define (compile-and-run exp)
  (let* ((compiled (compile exp 'val 'next))
         (insn-seq (statements compiled)))
    ;; TODO
    (for-each out (map car (extract-operations insn-seq)))
    ))
