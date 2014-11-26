(load "./legacy_lower_patch.scm")
(load "./gc-transform.scm")

(define default-primitive-list
  (let ((old-primitive-list default-primitive-list))
    (lambda ()
      `((broken-heart? ,(lambda (sym)
                          (eq? sym gc-broken-heart)))
        (debug-gc-start ,(lambda ()
                           (out "GC triggered")))
        ,@(old-primitive-list)))))

;; "the-stack" should be stored somewhere
;; in the memory, therefore we no longer want to
;; keep it reserved
(define reserved-registers
  '(pc flag
    the-cars the-cdrs
    new-cars new-cdrs))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           ;; the primitive "initialize-stack"
           ;; has been replaced with a lower level rewrite,
           ;; no need to keep it.
           '())
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
      (set! the-ops
            (cons (list 'debug-gc-end
                        (lambda ()
                          (format
                           #t
                           "GC done (~A/~A live cells)~%"
                           (pointer-get
                            (get-contents
                             (lookup-register 'free)))
                           memory-size)))
                  the-ops))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               ;; initialize some reserved registers here
               ;; when the machine starts
               (set-contents!
                (lookup-register 'free)
                (pointer 0))
               (set-contents!
                (lookup-register 'the-cars)
                (make-vector memory-size))
               (set-contents!
                (lookup-register 'the-cdrs)
                (make-vector memory-size))
               (set-contents!
                (lookup-register 'new-cars)
                (make-vector memory-size))
               (set-contents!
                (lookup-register 'new-cdrs)
                (make-vector memory-size))
               (set-contents!
                (lookup-register 'the-stack)
                '())
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

(define memory-size 512)
;; TODO: quick and dirty
(define machine-memory-size
  memory-size)

(define tranform-instructions
  gc-transform-program)

;; Local variables:
;; proc-entry: "./legacy_gc_patch_tests.scm"
;; End:
