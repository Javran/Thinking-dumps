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
              (out (car (car insns)))
              'skipped)
          ((cdr (car insns)))
          (machine-execute! m)))))
