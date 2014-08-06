(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./data-directed.scm")

(define (controller-description? c)
  (and (list? c)
       (not (null? c))
       (eq? 'controller (car c))))

(load "./exercise_5_5_machine_state.scm")
(load "./exercise_5_5_handlers.scm")

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

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
