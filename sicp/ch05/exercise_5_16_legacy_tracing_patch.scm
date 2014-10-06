(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; tracing flag as a variable
        (trace #f))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;; two primitives for tracing flag control
                 (list 'trace-on
                       (lambda () (set! trace #t)))
                 (list 'trace-off
                       (lambda () (set! trace #f)))))
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
              ;; 3 messages for tracing flag control
              ((eq? message 'trace?) trace)
              ((eq? message 'trace-on)
               (set! trace #t))
              ((eq? message 'trace-off)
               (set! trace #f))
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))

(define (trace-on! m)
  (m 'trace-on))
(define (trace-off! m)
  (m 'trace-off))
(define (trace-flag? m)
  (m 'trace?))
