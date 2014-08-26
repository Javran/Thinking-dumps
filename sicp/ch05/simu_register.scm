(define (new-register)
  (vector '*unassigned*))
(define (register-get reg)
  (vector-ref reg 0))
(define (register-set! reg val)
  (vector-set! reg 0 val))

(define (test-registers)
  (let ((reg1 (new-register))
        (reg2 (new-register)))
    (register-set! reg1 'one)
    (register-set! reg2 2)
    (register-set! reg1 'two)

    (do-test
     register-get
     (list
      (mat reg1 'two)
      (mat reg2 2)))))

(if *simu-test*
    (test-registers)
    'skipped)
