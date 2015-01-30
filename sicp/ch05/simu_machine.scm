(load "./simu_utils.scm")
(load "./simu_register.scm")
(load "./simu_stack.scm")

;; ==== abstract machine operations
(define (empty-machine)
  (vector
   (empty-stack)                        ; 0: stack
   '()                                  ; 1: instruction sequence
   '()                                  ; 2: register-table
   '()                                  ; 3: operations
   '()                                  ; 4: jump-table
   ;; extra-slot stores information that
   ;; can be accessed and updated by the machine
   '()                                  ; 5: extra-slot
   ))

;; internal use only, return machine field reference numbers
(define (machine-intern-ref symbol)
  (case symbol
    ((stack)                0)
    ((instruction-sequence) 1)
    ((register-table)       2)
    ((operations)           3)
    ((jump-table)           4)
    ((extra-slot)           5)
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
(define (machine-stack m)
  (vector-ref
   m
   (machine-intern-ref 'stack)))
(define (machine-instruction-sequence m)
  (vector-ref
   m
   (machine-intern-ref 'instruction-sequence)))
(define (machine-register-table m)
  (vector-ref
   m
   (machine-intern-ref 'register-table)))
(define (machine-operations m)
  (vector-ref
   m
   (machine-intern-ref 'operations)))
(define (machine-jump-table m)
  (vector-ref
   m
   (machine-intern-ref 'jump-table)))
(define (machine-extra-slot m)
  (vector-ref
   m
   (machine-intern-ref 'extra-slot)))

(define (machine-set-stack! m new-stack)
  (machine-intern-set-field! m 'stack new-stack))
(define (machine-set-instruction-sequence! m new-insn-seq)
  (machine-intern-set-field! m 'instruction-sequence new-insn-seq))
(define (machine-set-register-table! m new-reg-tab)
  (machine-intern-set-field! m 'register-table new-reg-tab))
(define (machine-set-operations! m new-ops)
  (machine-intern-set-field! m 'operations new-ops))
(define (machine-set-jump-table! m new-tbl)
  (machine-intern-set-field! m 'jump-table new-tbl))
(define (machine-set-extra-slot! m new-info)
  (machine-intern-set-field! m 'extra-slot new-info))

;; indirect accessors:

;; `regs-all` is a list of register names
;; allows duplicate register names
;; and 'pc and 'flag arg allowed to appear in this list
;; this procedure will take care of them
;; NOTE: 'pc and 'flag will always be defined
;; no matter whether they appear in the `regs-all` list or not
(define (machine-define-registers! m regs-all)
  ;; since "allocate-register" happens only at the creation of
  ;; a machine, we may just do it in one procedure
  ;; which we also have the benefit of detecting multiple defined registers
  ;; without too much pains
  (define regs
    (remove-duplicates
     `(pc flag ,@regs-all)))

  (machine-set-register-table!
   m
   (map (lambda (name)
          (list name (new-register)))
        regs)))

;; will raise an error when the register is not found
(define (machine-find-register m reg)
  (let ((reg-info (machine-lookup-register m reg)))
    (if reg-info
        (cadr reg-info)
        (error "register not defined:" reg))))

;; lookup the register, returns #f if the register is not found
;; otherwise the register binding structure is returned
(define (machine-lookup-register m reg)
  (assoc reg (machine-register-table m)))

(define (machine-reg-get m reg)
  (register-get (machine-find-register m reg)))

(define (machine-reg-set! m reg val)
  (let ((reg-infol (machine-lookup-register m reg)))
    (if reg-infol
        (register-set! (cadr reg-infol) val)
        ;; if the register is not found, we create one.
        (let ((new-reg-info (list reg (new-register))))
          (machine-set-register-table!
           m
           ;; here it is safe to assume that
           ;; this "reg" is a fresh name
           ;; or otherwise we should not be in this branch
           (cons new-reg-info (machine-register-table m)))
          (register-set! (cadr new-reg-info) val)))))

(define (machine-reset-pc! m)
  (machine-reg-set!
   m 'pc (machine-instruction-sequence m)))

(define assembled-insn-text car)
(define assembled-insn-proc cdr)

(define (machine-execute! m)
  (let ((insns (machine-reg-get m 'pc)))
    (if (null? insns)
        'done
        (begin
          ((assembled-insn-proc (car insns)))
          (machine-execute! m)))))

(define (machine-lookup-label m label)
  (let ((new-insn-pos (assoc label (machine-jump-table m))))
    (if new-insn-pos
        (cadr new-insn-pos)
        (error "label not found:" label))))

(define (machine-lookup-prim m prim)
  (let ((result (assoc prim (machine-operations m))))
    (if result
        (cadr result)
        (error "primtive not found:" prim))))

;; it might be more efficient
;; to find the register when assembling
;; but I guess this is not a big deal
;; as in our model, "pc" appears before
;; many other registers and therefore can
;; be found in a short time
(define (advance-pc m)
  (machine-reg-set!
   m 'pc
   (cdr (machine-reg-get m 'pc))))

;; initialize registers
(define (machine-init-regs!
         m init-reg-table)
  ;; we can only initialize those whose value is not given
  ;; in the table, but set operations might cost more

  ;; clean up all values
  (for-each
   (lambda (reg-name)
     (machine-reg-set! m reg-name '*unassigned*))
   (map car (machine-register-table m)))

  ;; set values
  (for-each
   (lambda (pair)
     ;; no need to check if the register exists
     ;; setting values to any undefined regster results
     ;; in a register not found error
     (machine-reg-set! m (car pair) (cadr pair)))
   init-reg-table))

;; initialize 'pc register and start machine execution
(define (machine-fresh-start! m)
  (machine-reset-pc! m)
  (machine-execute! m))

;; retrieve extra data from the machine
;; if the specified key does not match with
;; any given extra data, a default value is returned
(define (machine-extra-get m key default)
  (let ((result (assoc key (machine-extra-slot m))))
    (if result
        (cadr result)
        default)))

;; store extra data with a key on the machine
(define (machine-extra-set! m key value)
  (let* ((alist (machine-extra-slot m))
         (new-alist (cons (list key value)
                          (del-assoc key alist))))
    (machine-set-extra-slot! m new-alist)))

;; modify the stored extra data indicated by a key
;; if the old value cannot be found, default will
;; be used as if it was the old value
;; i.e. "proc" will take "default" as an *INPUT*
;; if no old value is found
(define (machine-extra-modify! m key proc default)
  (machine-extra-set!
   m key
   (proc (machine-extra-get m key default))))

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:
