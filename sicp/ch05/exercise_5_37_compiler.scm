(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
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
         seq2))))

(define (check-instruction-sequence compiled-seq)
  (let ((insn-seq (statements compiled-seq))
        (needed (registers-needed compiled-seq)))
    ;; * superfluous registers are kept
    ;;   because the "preserving" will no longer
    ;;   try to add as fewer "save" and "restore"
    ;;   as possible
    ;;   therefore the verification step is removed temporarily

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

;; Local variables:
;; proc-entry: "./exercise_5_37.scm"
;; End:
