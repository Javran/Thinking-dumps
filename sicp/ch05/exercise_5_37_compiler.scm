(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        ;; always apply register preserving
        (preserving
             (cdr regs)
             (make-instruction-sequence
              ;; register pulled in by "(save <reg>)"
              (list-union (list first-reg)
                          (registers-needed seq1))
              ;; register whose modification is cancelled by
              ;; "(save <reg>)" and "(restore <reg>)"
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)

;; Local variables:
;; proc-entry: "./exercise_5_37.scm"
;; End:
