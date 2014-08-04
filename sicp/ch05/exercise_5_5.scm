(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (controller-description? c)
  (and (list? c)
       (not (null? c))
       (eq? 'controller (car c))))

;; let's define machine state structure
;; (list 'machine-state <insns> <jump-alist> <stack> <regs>)
;; where `insns` is the current list of pending instructions
;; `jump-alist` is a list of label-instruction list pairs
;; `stack` is the current stack
;; `regs` is the current registers
(define ms-insns cadr)
(define ms-jump-alist caddr)
(define (ms-get-insns label ms)
  (cadr (assoc label (ms-jump-alist ms))))
(define ms-stack cadddr)
(define (ms-stack-top ms)
  (car (ms-stack ms)))
(define (ms-stack-pop ms)
  (list 'machine-state
        (ms-insns ms)
        (ms-jump-alist ms)
        (cdr (ms-stack ms))
        (ms-regs ms)))
(define (ms-stack-push v ms)
  (list 'machine-state
        (ms-insns ms)
        (ms-jump-alist ms)
        (cons v (ms-stack ms))
        (ms-regs ms)))
(define ms-regs  (compose car cddddr))
(define (ms-get-reg name ms)
  (let ((result (assoc name (ms-regs ms))))
    (if result
        (cadr result)
        #f)))

(define (ms-set-reg name val ms)
  (list 'machine-state
        (ms-insns ms)
        (ms-jump-alist ms)
        (ms-stack ms)
        (cons (list name val)
              (del-assoc name (ms-regs ms)))))

;; accepts a list of instructions
(define (make-jump-alist instructions)
  (let loop ((insns instructions)
             (jump-alist '()))
    (if (null? insns)
        jump-alist
        (if (symbol? (car insns))
            ;; add a new label
            (loop
             (cdr insns)
             (cons (list (car insns) (cdr insns))
                   jump-alist))
            ;; skip it
            (loop
             (cdr insns)
             jump-alist)))))

;; instead of hand-simulation, let's try to write one simulator
(define (run-machine controller-desc)
  (let ((instructions (cdr controller-desc)))
    (let ((label-insn-alist (make-jump-alist instructions)))
      'todo)))

(define (handle-save body ms)
  (let* ((var (car body))
         (val (ms-get-reg var ms)))
    (ms-stack-push val ms)))

(define (handle-restore body ms)
  (let* ((var (car body))
         (val (ms-stack-top ms)))
    (ms-set-reg var val
                (ms-stack-pop ms))))

(out (handle-save '(n) (list 'machine-state '(a b c) '() '(1 2 3) '((x 100) (n 400)))))
(out (handle-restore '(new-var)
                     (list 'machine-state '(a b c) '() '(1 2 3) '((x 100)))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
