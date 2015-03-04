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

(define ex-5-15-ops-builder
  (ops-builder-union
   (lambda (m)
     `((print-insn-counter
        ,(lambda ()
           (format
            #t "# instruction executed: ~A~%"
            (machine-instruction-counter m))))
       (reset-insn-counter
        ,(lambda ()
           (machine-reset-instruction-counter! m)))))
   default-ops-builder))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   ex-5-15-ops-builder))
