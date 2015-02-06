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
      ;; in the original code, "env" is one of the required register
      ;; as we can see in the instruction sequence it gets overwritten
      ;; immediately by a register assignment, so I guess
      ;; it's safe to drop this "env" from the required set of registers
      ;; (without preserving "env", we can still pass basic tests
      ;; I guess it's safe now)
      '(proc argl)
      '(env)
      `(,proc-entry
        (assign env
                (op compiled-procedure-env)
                (reg proc))
        ;; extend the embedded environment
        ;; to include argument bindings
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
  ;; inlining "construct-arglist" because
  ;; I haven't seen any usage other than one in
  ;; "compile-application"
  ;; takes a list of compiled operand-codes
  ;; and initialize "argl" properly
  (define (construct-arglist operand-codes)
    ;; compiling the argument list is a little bit tricky
    ;; because instead of "wasting an instruction by initializing
    ;; 'argl' to the empty list", the book wastes a few pages
    ;; explaining the compilcation incurred by using this weird order of
    ;; argument evaluation. And I wasted few lines here complaining about it.
    (let ((operand-codes (reverse operand-codes)))
      ;; NOTE: from now on (inside this s-exp)
      ;; the "operand-codes" is reverded.
      (if (null? operand-codes)
          ;; no more operands are required, simply
          ;; assigning "argl" an empty list to continue
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
                ;; this is the last argument
                code-to-get-last-arg
                ;; this is not the last argument
                (preserving
                 ;; all final linkages of subexpressions are just "next"s
                 ;; no need for keeping "continue" anyway.
                 ;; this can be tested by evaluating some function application
                 ;; whose operands are again function applications.
                 '(env)
                 code-to-get-last-arg
                 ;; merge in rest of the argument evaluations
                 (code-to-get-rest-args
                  (cdr operand-codes))))))))
  ;; inlining this procedure because there's no good reason
  ;; to leave it outside. Since "operand-codes" passed to it
  ;; is always reversed, reading the code without context
  ;; doesn't make any sense.
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
  ;; ====
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
      ;; dispatch to either a primitive procedure or composite one
      (compile-procedure-call target linkage)))))

;; apply arguments in "argl" to a procdure in "proc"
;; dispatches accordingly to the value of "proc"
(define (compile-procedure-call target linkage)
  ;; function application for compiled procedures
  (define (compile-proc-appl target linkage)
    ;; note that the linkage will never be "next"
    ;; case coverage:
    ;;         | target = val | target != val
    ;; return  | covered      | error
    ;; <other> | covered      | covered
    (cond ((and (eq? target 'val)
                (not (eq? linkage 'return)))
           ;; - no need to move result value
           ;; - do an immediate jump afterwards
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,linkage))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val)))))
          ((and (not (eq? target 'val))
                (not (eq? linkage 'return)))
           ;; - need to move result value "val" to the target
           ;; - do an immediate jump afterwards
           (let ((proc-return (make-label 'proc-return)))
             (make-instruction-sequence
              '(proc) all-regs
              `((assign continue (label ,proc-return))
                (assign val (op compiled-procedure-entry)
                        (reg proc))
                (goto (reg val))
                ,proc-return
                ;; need this extra step to transfer value
                (assign ,target (reg val))
                (goto (label ,linkage))))))
          ((and (eq? target 'val) (eq? linkage 'return))
           ;; - no need to move result value
           ;; - let the compiled procedure do the return job
           (make-instruction-sequence
            '(proc continue) all-regs
            `((assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val)))))
          ((and (not (eq? target 'val))
                (eq? linkage 'return))
           ;; - conversion is broken here: whenever we return,
           ;;   the resulting value should be available in "val"
           ;; - the only place where a "return" linkage used
           ;;   is in the body of "compile-lambda-body",
           ;;   and that code sets its target to "val".
           ;; so no code (at least for those found in book)
           ;; will hit this branch
           (error "return linkage, target not val: COMPILE"
                  target))))
  ;; ====
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          ;; goto primitive branch
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         ;; otherwise the procedure must be a compiled one
         compiled-branch
         ;; note that it's not possible for compiled-linkage to
         ;; take value "next"
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
