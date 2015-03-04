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

;; considering the issue of modulality, we make the decision that
;; ops-builder should not accumulate
(define (ex-5-15-ops-builder-extra m)
  `((print-insn-counter
     ,(lambda ()
        (format
         #t "# instruction executed: ~A~%"
         (machine-instruction-counter m))))
    (reset-insn-counter
     ,(lambda ()
        (machine-reset-instruction-counter! m)))))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   (ops-builder-union
    ex-5-15-ops-builder-extra
    default-ops-builder)))
