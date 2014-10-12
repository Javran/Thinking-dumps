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

(define (register-trace-on! reg)
  ((reg 'set-trace!) #t))
(define (register-trace-off! reg)
  ((reg 'set-trace!) #f))
(define (register-tracing? reg)
  (reg 'trace?))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;; reg trace on/off as primitives
                 (list 'trace-reg-on
                       (lambda (name)
                         (register-trace-on!
                          (lookup-register name))))
                 (list 'trace-reg-off
                       (lambda (name)
                         (register-trace-off!
                          (lookup-register name))))
                 ))
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register:"
                   name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:"
                     name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))
