;; TODO: well, I know it looks stupid, but don't worry about it for now
;; TODO: some exp compilers might not require ctenv at all
;; therefore we can simplify a little bit
;; (but we might start to worry about the consistency of
;; arguments later ...)

(define (compile exp target linkage ctenv)
  (cond
   ((self-evaluating? exp)
    (compile-self-evaluating exp target linkage ctenv))
   ((quoted? exp)
    (compile-quoted exp target linkage ctenv))
   ((variable? exp)
    (compile-variable exp target linkage ctenv))
   ((assignment? exp)
    (compile-assignment exp target linkage ctenv))
   ((definition? exp)
    (compile-definition
     (normalize-define exp)
     target linkage ctenv))
   ((if? exp)
    (compile-if exp target linkage ctenv))
   ((lambda? exp)
    (compile-lambda exp target linkage ctenv))
   ((begin? exp)
    (compile-sequence
     (begin-actions exp) target linkage ctenv))
   ((cond? exp)
    (compile (cond->if exp) target linkage ctenv))
   ((let? exp)
    (compile (let->combination exp) target linkage ctenv))
   ((application? exp)
    (compile-application exp target linkage ctenv))
   (else
    (error "Unknown expression type: COMPILE" exp))))

(define (compile-if exp target linkage ctenv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next)
               after-if
               linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next ctenv))
            (c-code (compile (if-consequent exp)
                             target
                             consequent-linkage
                             ctenv))
            (a-code (compile (if-alternative exp)
                             target
                             linkage
                             ctenv)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-self-evaluating exp target linkage ctenv)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage ctenv)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage ctenv)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env)
    (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

(define (compile-assignment exp target linkage ctenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ctenv)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage ctenv)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next ctenv)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-sequence seq target linkage ctenv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage ctenv)
      (preserving
       '(env continue)
       (compile (first-exp seq) target 'next ctenv)
       (compile-sequence (rest-exps seq) target linkage ctenv))))

;; TODO: procedures
(define (compile-lambda exp target linkage ctenv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry ctenv))
       after-lambda))))

(define (compile-lambda-body exp proc-entry ctenv)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(proc argl)
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
     (compile-sequence (lambda-body exp) 'val 'return ctenv))))

(define (compile-application exp target linkage ctenv)
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
  (let ((proc-code (compile (operator exp) 'proc 'next ctenv))
        (operand-codes
         (map
          (lambda (operand)
            (compile operand 'val 'next ctenv))
          (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage ctenv)))))

(define (compile-procedure-call target linkage ctenv)
  (define (compile-proc-appl target linkage ctenv)
    (cond ((and (eq? target 'val)
                (not (eq? linkage 'return)))
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
            '(proc continue) all-regs
            `((assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val)))))
          ((and (not (eq? target 'val))
                (eq? linkage 'return))
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
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage ctenv))
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
