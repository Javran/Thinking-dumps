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



(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
