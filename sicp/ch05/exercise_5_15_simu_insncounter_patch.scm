(define (machine-execute! m)
  (let ((insns (machine-reg-get m 'pc)))
    (if (null? insns)
        'done
        (begin
          ((cdr (car insns)))
          ;; after an instruction is execute,
          ;; we bump the counter
          (machine-extra-modify!
           m 'instruction-counter add1 0)
          (machine-execute! m)))))

(define (machine-instruction-counter m)
  (machine-extra-get m 'instruction-counter 0))

(define (machine-reset-instruction-counter! m)
  (machine-extra-set! m 'instruction-counter 0))

(define default-ops-builder
  ;; should be safe if the expression
  ;; in "define" form gets evaluates immediately
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `((print-insn-counter
         ,(lambda ()
           (format
            #t "# instruction executed: ~A~%"
            (machine-instruction-counter m))))
        (reset-insn-counter
         ,(lambda ()
           (machine-reset-instruction-counter! m)))
        ,@(old-builder m)))))
