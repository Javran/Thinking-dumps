(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")

(set-handler
 'assign
 (lambda (insn m)
   (error "assign instruction is not allowed"
          "in this new syntax")))

(define (copy-handler insn m)
  (let* ((target-name
          (cadr insn))
         (target
          (machine-find-register m target-name))
         (value-proc
          (make-primitive-exp
           (caddr insn) m)))
    (lambda ()
      (register-set! target (value-proc))
      (advance-pc m))))
(set-handler 'copy copy-handler)

(define (call-handler insn m)
  (let* ((target-name (cadr insn))
         (target
          (machine-find-register m target-name))
         (prim-exp (caddr insn))
         (arg-exps (cdddr insn))
         (value-exp
          `((op ,prim-exp) ,@arg-exps))
         (value-proc
          (make-operation-exp
           (operation-exp-op value-exp)
           (operation-exp-operands value-exp)
           m)))
    (lambda ()
      (register-set! target (value-proc))
      (advance-pc m))))
(set-handler 'call call-handler)

(define (extract-register-names instructions)
  (define (extract insn)
    (cond
     ((or (tagged-list? insn 'copy)
          (tagged-list? insn 'call)
          (tagged-list? insn 'restore))
      (list (cadr insn)))
     (else '())))
  (remove-duplicates
   (concat-map extract instructions)))

(load "./exercise_5_10_example_controller.scm")

(let ((m (build-and-execute
          example-controller
          '())))
  (for-each
   (lambda (r)
     (out (machine-reg-get m r)))
   '(a b c)))

(end-script)
