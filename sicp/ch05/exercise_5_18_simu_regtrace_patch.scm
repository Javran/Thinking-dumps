(define (new-register)
  ;; adding a new field for register-setter callback
  (vector '*unassigned* #f))

;; callback procedure:
;; (callback <register-object> <old-value> <new-value>)

(define (register-setter-callback reg)
  (vector-ref reg 1))
(define (register-set-setter-callback! reg callback)
  (vector-set! reg 1 callback))

(define (register-set! reg val)
  (let ((old-val (register-get reg))
        (callback (register-setter-callback reg)))
    (if callback
        (callback reg old-val val)
        'skipped)
    (vector-set! reg 0 val)))

(define (machine-register-set-trace! m name flag)
  (let ((reg-obj (machine-find-register m name))
        (callback (lambda (reg-obj old-val new-val)
                    (format #t "reg: ~A~%~
                                old-val: ~A~%~
                                new-val: ~A~%"
                            name
                            old-val
                            new-val))))
    (register-set-setter-callback!
     reg-obj
     (if flag
         callback
         #f))))

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


