(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./data-directed.scm")

(define (controller-description? c)
  (and (list? c)
       (not (null? c))
       (eq? 'controller (car c))))


(load "./exercise_5_5_machine_state.scm")

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
         (val (ms-reg-get var ms)))
    (ms-stack-push val ms)))

(define (handle-restore body ms)
  (let* ((var (car body))
         (val (ms-stack-top ms)))
    (ms-reg-set var val
                (ms-stack-pop ms))))

(define (handle-goto body ms)
  (let ((type (caar body))
        (arg  (cadar body)))
    (let ((lbl
           (cond
            ((eq? type 'reg)
             (ms-reg-get arg ms))
            ((eq? type 'label)
             arg)
            (else
             (error "unknown type:"
                    type)))))
      (ms-insns-set
       (ms-query-label lbl ms)
       ms))))

(define (handle-branch body ms)
  (let ((type (caar body))
        (arg  (cadar body)))
    (let ((lbl
           (cond
            ((eq? type 'reg)
             (ms-reg-get arg ms))
            ((eq? type 'label)
             arg)
            (else
             (error "unknown type:"
                    type)))))
      (if (ms-test-flag ms)
          (ms-insns-set
           (ms-query-label lbl ms)
           ms)
          ms))))

(define (handle-test body ms)
  (let ((type (caar body))
        (arg  (cadar body))
        (args (cdr body)))
    (define (get-value data)
      (let ((type (car data))
            (arg  (cadr data)))
        (cond
         ((eq? type 'const) arg)
         ((eq? type 'reg) (ms-reg-get arg ms))
         (else
          (error "unknown type"
                 type)))))
    (assert (eq? type 'op))
    (let ((operator (eval arg user-initial-environment))
          (operands (map get-value args)))
      (ms-test-flag-set
       (apply operator operands)
       ms))))

(define (handle-assign body ms)
  (let ((target (car body))
        (type (caadr body))
        (arg  (cadadr body))
        (args (cddr body)))
    (define (get-value data)
      (let ((type (car data))
            (arg  (cadr data)))
        (cond
         ((eq? type 'const) arg)
         ((eq? type 'reg) (ms-reg-get arg ms))
         (else
          (error "unknown type"
                 type)))))
    (let ((new-val
           (cond
            ((eq? type 'label) arg)
            ((eq? type 'const) arg)
            ((eq? type 'reg) (ms-reg-get arg ms))
            ((eq? type 'op)
             (let ((operator (eval arg user-initial-environment))
                   (operands (map get-value args)))
               (apply operator operands)))
            (else
             (error "unknown type"
                    type)))))
      (ms-reg-set target new-val ms))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
