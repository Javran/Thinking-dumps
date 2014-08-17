;; / ==== lightweight implementation of a (pure) stack
(define (empty-stack) '())

(define stack-push cons)
(define stack-pop cdr)
(define stack-top car)
;; \ ====

;; / ==== abstract machine operations
(define (empty-machine)
  (vector
   '*unassigned* ; 0: PC
   '*unassigned* ; 1: flag
   (empty-stack) ; 2: stack
   '()           ; 3: empty instruction sequence
   '()           ; 4: register-table
   ))

;; internal use only, give machine fields
(define (machine-intern-ref symbol)
  (case symbol
    ((pc) 0)
    ((flag) 1)
    ((stack) 2)
    ((instruction-sequence) 3)
    ((register-table) 4)
    (else (error "MACHINE: unknown internal ref: "
                 symbol))))
(define (machine-intern-field m sym)
  (vector-ref
   m
   (machine-intern-ref sym)))
(define (machine-intern-set-field! m sym new-val)
  (vector-set!
   m
   (machine-intern-ref sym)
   new-val))

;; accessors

;; direct accessors: machine-<field-name>
(define machine-pc
  ((curry2 vector-ref)
   (machine-intern-ref 'pc)))
(define machine-flag
  ((curry2 vector-ref)
   (machine-intern-ref 'flag)))
(define machine-stack
  ((curry2 vector-ref)
   (machine-intern-ref 'stack)))
(define machine-instruction-sequence
  ((curry2 vector-ref)
   (machine-intern-ref 'instruction-sequence)))
(define machine-register-table
  ((curry2 vector-ref)
   (machine-intern-ref 'register-table)))

(define (machine-set-pc! m new-pc)
  (machine-intern-set-field! m 'pc new-pc))
(define (machine-set-flag! m new-flag)
  (machine-intern-set-field! m 'flag new-flag))
(define (machine-set-stack! m new-stack)
  (machine-intern-set-field! m 'stack new-stack))
(define (machine-set-instruction-sequence! m new-insn-seq)
  (machine-intern-set-field! m 'instruction-sequence new-insn-seq))
(define (machine-set-register-table! m new-reg-tab)
  (machine-intern-set-field! m 'register-table new-reg-tab))

;; indirect accessors:
;; TODO: we need to work with registers first
(define (machine-allocate-registers! m regs)
  ;; since "allocate-register" happens only at the creation of
  ;; a machine, we may just do it in one procedure
  ;; which we also have the benefit of detecting multiple defined registers
  ;; without too much pains
  'todo)

;; \ ====

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
