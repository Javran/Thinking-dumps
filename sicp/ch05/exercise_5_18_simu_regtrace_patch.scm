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
  (member
   reg
   (machine-extra-get m 'tracing-regs '())))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `((trace-reg-on
         ,(lambda (name)
            (machine-register-set-trace! m name #t)))
        (trace-reg-off
         ,(lambda (name)
            (machine-register-set-trace! m name #f)))
        ,@(old-builder m)))))

(define (machine-reg-set! m reg val)
  (let ((reg-obj (machine-find-register m reg)))
    (if (machine-register-tracing? m reg)
        (let ((old-value (register-get reg-obj)))
          (format #t  "reg: ~A~%~
                       old-val: ~A~%~
                       new-val: ~A~%"
                  reg
                  old-value
                  val))
        'skipped)
    (register-set! reg-obj val)))
