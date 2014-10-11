(define (make-register name)
  (let ((contents '*unassigned*)
        ;; introduce a flag in scope
        (trace? #f))
    (define (dispatch message)
      (cond
       ((eq? message 'get) contents)
       ((eq? message 'set)
        (lambda (value)
          ;; test the flag
          ;; and print out the message as suggested
          (let ((old-value contents))
            (if trace?
                (format #t "reg: ~A~%~
                            old-val: ~A~%~
                            new-val: ~A~%"
                        name
                        old-value
                        value)
                'skipped)
            (set! contents value))))
       ((eq? message 'trace?) trace?)
       ((eq? message 'set-trace!)
        (lambda (v)
          (set! trace? v)))
       (else
        (error "unknown request: REGISTER"
               message))))
    dispatch))
