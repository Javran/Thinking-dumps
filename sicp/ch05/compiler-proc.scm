;; a module that contains procedure-related codes

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               ;; the body potentially contains branches
               ;; in which case we need a "after-lambda"
               ;; to jump to the correct exit point
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        ;; the compiled body has nothing to do with
        ;; procedure-creation code, but we tack the compiled
        ;; procedure body right after the procedure creation code
        ;; and proper jump is used to make things correct
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          ;; needs to store the current environment
          ;; and assigns the compiled procedure to target register
          '(env) (list target)
          ;; a compiled procedure
          ;; consists of:
          ;; - the label to the procedure entry
          ;; - the environment in which it was created
          ;; - at runtime, the arguments are passed through
          ;;   "argl" register
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

;; constructor & accessors of compiled procedure structure
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

;; compile the body of a lambda expression
;; assuming "argl" register is well initialized
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     ;; the initial part of the compiled code
     (make-instruction-sequence
      ;; TODO: not sure why "env" is required here
      ;; as in the code it is overwritten immediately
      ;; by a register assignment - maybe it's safe to remove it
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     ;; compile the body of a lambda expression
     (compile-sequence (lambda-body exp) 'val 'return))))

;; compile function application
;; in an applicative order evaluation, this part is responsible
;; for properly initializing "argl" register
;; to fully evaluation arguments
(define (compile-application exp target linkage)
  ;; inline "construct-arglist" because
  ;; I haven't seen any usage other than one in
  ;; "compile-application"
  ;; takes a list of compiled operand-codes
  ;; and initialize "argl" properly
  (define (construct-arglist operand-codes)
    (let ((operand-codes (reverse operand-codes)))
      (if (null? operand-codes)
          (make-instruction-sequence
           '() '(argl)
           '((assign argl (const ()))))
          (let ((code-to-get-last-arg
                 (append-instruction-sequences
                  (car operand-codes)
                  (make-instruction-sequence
                   '(val) '(argl)
                   '((assign argl (op list) (reg val)))))))
            (if (null? (cdr operand-codes))
                code-to-get-last-arg
                (preserving
                 '(env)
                 code-to-get-last-arg
                 (code-to-get-rest-args
                  (cdr operand-codes))))))))
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map
          (lambda (operand)
            (compile operand 'val 'next))
          (operands exp))))
    (preserving
     '(env continue)
     ;; evaluate operator
     proc-code
     (preserving
      '(proc continue)
      ;; evaluate operands
      (construct-arglist operand-codes)
      ;; ???
      (compile-procedure-call target linkage)))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl
                     (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
        after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          `((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, target not val: COMPILE"
                target))))
