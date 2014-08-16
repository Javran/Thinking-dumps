(define (make-machine register-names
                      ops
                      controller-text)
  ;; register-names: a list of register names
  ;;   to be used in this machine model
  ;; ops: an "alist" of operation records
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; TODO:
;; I don't see why it is necessary to make stateful objects
;; the following code basically wraps everything inside,
;; a dispatch function is used to access these functions,
;; and all these procedures has a globally defined procedure
;; which calls the object with suitable symbol.
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           ;; will this pollute the namespace?
           (list (list 'pc pc)
                 (list 'flag flag))))
      ;; allocate a new register
      ;; a unique name is enforced
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: "
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
              ((eq? message 'install-operationsp)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))
