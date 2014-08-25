(load "./data-directed.scm")

;; ==== build handler lookup table

(define set-handler #f)
(define get-handler #f)
(define init-handler-table! #f)

(let* ((f-alist (global-table-functions))
       (set1 (cadr (assoc 'set f-alist)))
       (get1 (cadr (assoc 'get f-alist)))
       (init1 (cadr (assoc 'init f-alist))))
  (set! set-handler set1)
  (set! get-handler get1)
  (set! init-handler-table! init1))

;; ====
;; * (set-handler <slot> <handler>) to set a handler
;; * (get-handler <slot>) to retrieve a handler

;; analyze the list of instructions once for all,
;; yielding "thunks" which can be used multiple times
;; without too much time consumption.

(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp)
  (cadr exp))

(define (make-primitive-exp exp m)
  ;; accessors
  (define (constant-exp? exp)
    (tagged-list? exp 'const))
  (define (constant-exp-value exp)
    (cadr exp))

  (define (register-exp? exp)
    (tagged-list? exp 'reg))
  (define (register-exp-reg exp)
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

(define (make-operation-exp exp m)
  (let ((op (machine-lookup-prim
             m (operation-exp-op exp)))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e m))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (advance-pc m)
  (machine-reg-set!
   m 'pc
   (cdr (machine-reg-get m 'pc))))

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
                value-exp m)
               (make-primitive-exp
                (car value-exp) m))))
      (lambda ()
        (register-set! target-reg (value-proc))
        (advance-pc m)))))
(set-handler 'assign assign-handler)

(define (test-handler insn m)

  (let ((condition (test-condition insn)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition m)))
          (lambda ()
            ;; execute the "operation", and then set the flag
            (machine-reg-set! m 'flag (condition-proc))
            (advance-pc m)))
        (error "bad instruction:"
               insn))))
(set-handler 'test test-handler)

(define (branch-handler insn m)
  (let ((dest (branch-dest insn)))
    ;; must be a label (might extend to "goto"
    ;; but this functionality is not seen in the book)
    (if (label-exp? dest)
        (lambda ()
          (let ((insns
                 (machine-lookup-label
                  m (label-exp-label dest))))
            ;; check the flag first and then make the decision
            ;; of either jumping or advancing pc
            (if (machine-reg-get m 'flag)
                (machine-reg-set! m 'pc insns)
                (advance-pc m))))
        ;; just wondering why we need to print out these
        ;; error info while the error only happens when
        ;; being used internally.
        (error "bad instruction:" insn))))
(set-handler 'branch branch-handler)

#|
(define (goto-handler inst labels machine pc flag stack ops)
  'todo)

(define (save-handler inst labels machine pc flag stack ops)
  'todo)

(define (restore-handler inst labels machine pc flag stack ops)
  'todo)

(define (perform-handler inst labels machine pc flag stack ops)
  'todo)
|#

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:
