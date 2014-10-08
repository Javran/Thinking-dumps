(define (machine-trace? m)
  ;; trace flag is turned off by default
  (machine-extra-get m 'trace #f))

(define (machine-trace-on! m)
  (machine-extra-set! m 'trace #t))
(define (machine-trace-off! m)
  (machine-extra-set! m 'trace #f))

(define (machine-execute! m)
  (let ((insns (machine-reg-get m 'pc)))
    (if (null? insns)
        'done
        (begin
          ;; print tracing message before
          ;; the instruction gets executed
          (if (machine-trace? m)
              (out (assembled-insn-text (car insns)))
              'skipped)
          ((assembled-insn-proc (car insns)))
          (machine-execute! m)))))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `((trace-on
         ,(lambda ()
            (machine-trace-on! m)))
        (trace-off
         ,(lambda ()
            (machine-trace-off! m)))
        ,@(old-builder m)))))
