(load "./simu_utils.scm")
(load "./simu_register.scm")

;; / ==== lightweight implementation of a stack
(define (empty-stack) (vector '()))

(define (stack-push! st e)
  (vector-modify! st 0 (lambda (stack)
                         (cons e stack))))
(define (stack-pop! st)
  (vector-modify! st 0 cdr))
(define (stack-top st)
  (car (vector-ref 0 st)))
;; \ ====

;; / ==== abstract machine operations
(define (empty-machine)
  (vector
   (empty-stack)                        ; 0: stack
   '()                                  ; 1: instruction sequence
   '()                                  ; 2: register-table
   '()                                  ; 3: operations
   '()                                  ; 4: jump-table
   ))

;; internal use only, return machine field reference numbers
(define (machine-intern-ref symbol)
  (case symbol
    ((stack)                0)
    ((instruction-sequence) 1)
    ((register-table)       2)
    ((operations)           3)
    ((jump-table)           4)
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

;; indirect accessors:
;; `regs` is a list of register names
(define (machine-define-registers! m regs)
  ;; since "allocate-register" happens only at the creation of
  ;; a machine, we may just do it in one procedure
  ;; which we also have the benefit of detecting multiple defined registers
  ;; without too much pains

  ;; detect duplicated registers
  (let loop ((regs regs))
    (if (null? regs)
        'ok
        (let ((hd (car regs))
              (tl (cdr regs)))
          (if (memq hd tl)
              (error "duplicated register name:"
                     hd)
              (loop tl)))))

  (machine-set-register-table!
   m
   (map (lambda (name)
          (list name (new-register)))
        `(pc flag ,@regs))))

(define (machine-find-register m reg)
  (let ((reg-info (assoc reg (machine-register-table m))))
    (if reg-info
        (cadr reg-info)
        (error "register not defined:" reg))))

(define (machine-reg-get m reg)
  (register-get (machine-find-register m reg)))

(define (machine-reg-set! m reg val)
  (register-set! (machine-find-register m reg) val))

(define (machine-reset-pc! m)
  (machine-reg-set!
   m 'pc (machine-instruction-sequence m)))

(define (machine-execute! m)
  (let ((insns (machine-reg-get m 'pc)))
    (if (null? insns)
        'done
        (begin
          (format #t "next: ~A~%" (caar insns))
          ((cdr (car insns)))
          (machine-execute! m)))))

(define (machine-lookup-label m label)
  (let ((new-insn-pos (assoc label (machine-jump-table m))))
    (if new-insn-pos
        (cadr new-insn-pos)
        (error "label not found:" label))))

;; TODO: install-instruction-sequence
;;       execute

;; \ ====

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:
