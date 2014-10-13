(define (machine-register-set-trace! m reg flag)
  ;; the trace flag is set by maintaining a set of
  ;; tracing flags in the machine extra-data field
  (define (register-trace-on tracing-regs)
    ;; to set register flag is to add the register name into
    ;; that set, this "set" is allowed to contain duplicates
    ;; for simplicity.
    (cons reg tracing-regs))
  (define (register-trace-off tracing-regs)
    ;; to unset register flag is to remove the register name from
    ;; this set, "delete" is guaranteed to work even if the element
    ;; to be removed has duplicates.
    (delete reg tracing-regs))
  (machine-extra-modify!
   m
   'tracing-regs
   (if flag
       register-trace-on
       register-trace-off)
   '()))

(define (machine-register-tracing? m reg)
  (assoc
   reg
   (machine-extra-get m 'tracing-regs '())))
