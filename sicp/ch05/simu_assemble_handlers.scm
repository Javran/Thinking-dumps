(load "simu_assemble_handler_def.scm")
(load "simu_controller_syntax.scm")

(define (make-execution-procedure insn-text machine)
  (let ((handler (get-handler (car insn-text))))
    (if handler
        ;; we choose to keep arguments simple
        ;; as it is easier to understand and maintain.
        (handler insn-text machine)
        (error "unknown instruction:" insn-text))))

;; analyze the list of instructions once for all,
;; yielding "thunks" which can be used multiple times
;; without too much time consumption.

(define (make-primitive-exp exp m)
  ;; accessors
  (define (constant-exp? exp)
    (tagged-list? exp 'const))
  (define (constant-exp-value exp)
    (cadr exp))

  (cond ((constant-exp? exp)
         ;; (const <value>)
         (let ((c (constant-exp-value exp)))
           ;; we write in this way so that "c"
           ;; is known before the thunk get executed.
           (lambda () c)))
        ((label-exp? exp)
         ;; (label <label>)
         ;; be extreme careful here not to
         ;; query the jump-label from `assemble`
         ;; which is not ready at that time
         (lambda ()
           (machine-lookup-label
            m (label-exp-label exp))))
        ((register-exp? exp)
         ;; (reg <reg>)
         (let ((r (machine-find-register
                   m (register-exp-reg exp))))
           (lambda () (register-get r))))
        (else
         (error "unexpected expression:" exp))))

(define (make-operation-exp prim-exp arg-exps m)
  (let ((op (machine-lookup-prim
             m prim-exp))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e m))
              arg-exps)))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; handler type:
;; (<handler> insn machine)

;; NOTE: be careful when assigning something to pc,
;; as "assign" will always advance the pc register
(define (assign-handler insn m)
  ;; accessors for "assign"
  ;; (assign <reg> @<value-exp> ..)
  (define assign-reg-name cadr)
  (define assign-value-exp cddr)

  (let ((target-reg
         (machine-find-register m (assign-reg-name insn)))
        (value-exp
         (assign-value-exp insn)))
    (let ((value-proc
           ;; yields a value when run as a procedure
           (if (operation-exp? value-exp)
               (make-operation-exp
                (operation-exp-op value-exp)
                (operation-exp-operands value-exp)
                m)
               (make-primitive-exp
                (car value-exp) m))))
      (lambda ()
        (register-set! target-reg (value-proc))
        (advance-pc m)))))
(set-handler 'assign assign-handler)

(define (test-handler insn m)
  ;; (test @<condition>)
  (define test-condition cdr)

  (let ((condition (test-condition insn)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                (operation-exp-op condition)
                (operation-exp-operands condition)
                m)))
          (lambda ()
            ;; execute the "operation", and then set the flag
            (machine-reg-set! m 'flag (condition-proc))
            (advance-pc m)))
        (error "bad instruction:"
               insn))))
(set-handler 'test test-handler)

(define (branch-handler insn m)
  ;; (branch <destination>)
  (define branch-dest cadr)

  (let ((dest (branch-dest insn)))
    ;; must be a label (might extend to "goto"
    ;; but this functionality is not seen in the book)
    (if (label-exp? dest)
        (let ((flag-reg (machine-find-register m 'flag))
              (pc-reg (machine-find-register m 'pc)))
          (lambda ()
            (let ((insns
                   (machine-lookup-label
                    m (label-exp-label dest))))
              ;; check the flag first and then make the decision
              ;; of either jumping or advancing pc
              (if (register-get flag-reg)
                  (register-set! pc-reg insns)
                  (advance-pc m)))))
        ;; just wondering why we need to print out these
        ;; error info while the error only happens when
        ;; being used internally.
        (error "bad instruction:" insn))))
(set-handler 'branch branch-handler)

(define (goto-handler insn m)
  ;; (goto <destionation>)
  (define goto-dest cadr)

  (let ((dest (goto-dest insn)))
    (cond
     ((label-exp? dest)
      (let ((pc-reg (machine-find-register m 'pc)))
        (lambda ()
          (let ((insns
                 (machine-lookup-label
                  m (label-exp-label dest))))
            (register-set! pc-reg insns)))))
     ((register-exp? dest)
      (let ((pc-reg (machine-find-register m 'pc))
            (dest-reg (machine-find-register
                       m (register-exp-reg dest))))
        (lambda ()
          (register-set!
           pc-reg (register-get dest-reg)))))
     (else
      (error "bad instruction:" insn)))))
(set-handler 'goto goto-handler)

(define (save-handler insn m)
  (let ((reg (machine-find-register
              m (stack-insn-reg-name insn)))
        (stack (machine-stack m)))
    (lambda ()
      (stack-push! stack (register-get reg))
      (advance-pc m))))
(set-handler 'save save-handler)

(define (restore-handler insn m)
  (let ((reg (machine-find-register
              m (stack-insn-reg-name insn)))
        (stack (machine-stack m)))
    (lambda ()
      (register-set! reg (stack-top stack))
      (stack-pop! stack)
      (advance-pc m))))
(set-handler 'restore restore-handler)

(define (perform-handler insn m)
  ;; (perform @<inst>)
  (define (perform-action inst) (cdr inst))

  (let ((action (perform-action insn)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                (operation-exp-op action)
                (operation-exp-operands action)
                m)))
          (lambda ()
            (action-proc)
            (advance-pc m)))
        (error "bad instruction:" insn))))
(set-handler 'perform perform-handler)

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:
