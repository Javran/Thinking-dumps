;; this file just serves the purpose of comparison
;; with all comments, tests and shortcut functions removed
;; this implementation takes only 339 lines of code and is more powerful
;; (like defining new handlers with ease)
;; comparing with the legacy one, which is about 346 lines of code
;; (comment takes 2 lines)
(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./data-directed.scm")

(define (vector-modify! vec k proc)
  (vector-set! vec k (proc (vector-ref vec k))))

(define (remove-duplicates xs)
  (if (null? xs)
      '()
      (cons (car xs)
            (delete (car xs) (remove-duplicates (cdr xs))))))

(define (set-diff xs ys)
  (fold-right delete xs ys))

(define (same-length? xs ys)
  (cond ((null? xs) (null? ys))
        ((and (pair? xs) (pair? ys))
         (same-length? (cdr xs) (cdr ys)))
        (else #f)))

(define (list-tagged-with tag)
  (lambda (l)
    (and
      (list? l)
      (non-empty? l)
      (eq? (car l) tag))))

(define (tagged-list? exp tag)
  ((list-tagged-with tag) exp))

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

(define (new-register)
  (vector '*unassigned*))
(define (register-get reg)
  (vector-ref reg 0))
(define (register-set! reg val)
  (vector-set! reg 0 val))

(define (empty-stack) (vector '()))

(define (stack-push! st e)
  (vector-modify! st 0 (lambda (stack) (cons e stack))))
(define (stack-pop! st)
  (vector-modify! st 0 cdr))
(define (stack-top st)
  (car (vector-ref st 0)))

(define (empty-machine)
  (vector
   (empty-stack) '() '() '() '()))

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
  (vector-ref m (machine-intern-ref sym)))
(define (machine-intern-set-field! m sym new-val)
  (vector-set! m (machine-intern-ref sym) new-val))

(define (machine-stack m)
  (vector-ref m (machine-intern-ref 'stack)))
(define (machine-instruction-sequence m)
  (vector-ref m (machine-intern-ref 'instruction-sequence)))
(define (machine-register-table m)
  (vector-ref m (machine-intern-ref 'register-table)))
(define (machine-operations m)
  (vector-ref m (machine-intern-ref 'operations)))
(define (machine-jump-table m)
  (vector-ref m (machine-intern-ref 'jump-table)))

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

(define (machine-define-registers! m regs)
  (let loop ((regs regs))
    (if (null? regs)
        'ok
        (let ((hd (car regs))
              (tl (cdr regs)))
          (if (memq hd tl)
              (error "duplicated register name:"
                     hd)
              (loop tl)))))

  (machine-set-register-table! m
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
          ((cdr (car insns)))
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

(define (advance-pc m)
  (machine-reg-set!
   m 'pc
   (cdr (machine-reg-get m 'pc))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (label-exp? exp)
  (tagged-list? exp 'label))
(define label-exp-label cadr)

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define register-exp-reg cadr)

(define stack-insn-reg-name cadr)

(define (make-execution-procedure insn-text machine)
  (let ((handler (get-handler (car insn-text))))
    (if handler
        (handler insn-text machine)
        (error "unknown instruction:" insn-text))))

(define (make-primitive-exp exp m)
  (define (constant-exp? exp)
    (tagged-list? exp 'const))
  (define (constant-exp-value exp)
    (cadr exp))

  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (lambda ()
           (machine-lookup-label
            m (label-exp-label exp))))
        ((register-exp? exp)
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

(define (assign-handler insn m)
  (define assign-reg-name cadr)
  (define assign-value-exp cddr)

  (let ((target-reg
         (machine-find-register m (assign-reg-name insn)))
        (value-exp
         (assign-value-exp insn)))
    (let ((value-proc
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
  (define test-condition cdr)

  (let ((condition (test-condition insn)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition m)))
          (lambda ()
            (machine-reg-set! m 'flag (condition-proc))
            (advance-pc m)))
        (error "bad instruction:" insn))))
(set-handler 'test test-handler)

(define (branch-handler insn m)
  (define branch-dest cadr)

  (let ((dest (branch-dest insn)))
    (if (label-exp? dest)
        (let ((flag-reg (machine-find-register m 'flag))
              (pc-reg (machine-find-register m 'pc)))
          (lambda ()
            (let ((insns
                   (machine-lookup-label
                    m (label-exp-label dest))))
              (if (register-get flag-reg)
                  (register-set! pc-reg insns)
                  (advance-pc m)))))
        (error "bad instruction:" insn))))
(set-handler 'branch branch-handler)

(define (goto-handler insn m)
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
  (define (perform-action inst) (cdr inst))

  (let ((action (perform-action insn)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action m)))
          (lambda ()
            (action-proc)
            (advance-pc m)))
        (error "bad instruction:" insn))))
(set-handler 'perform perform-handler)

(define (assemble controller-text machine)
  (define (make-instruction insn-text)
    (if (symbol? insn-text)
        insn-text
        (cons insn-text
              (make-execution-procedure insn-text machine))))

  (define (drop-labels insns)
    (filter (compose not symbol?) insns))

  (let ((insns (map make-instruction controller-text)))
    (let ((jump-table
           (let loop ((table '())
                      (insns insns))
             (if (null? insns)
                 table
                 (let ((hd (car insns))
                       (tl (cdr insns)))
                   (if (symbol? hd)
                       (loop (cons (list hd (drop-labels tl)) table) tl)
                       (loop table tl)))))))
      (machine-set-instruction-sequence! machine (drop-labels insns))
      (machine-set-jump-table! machine jump-table))))
