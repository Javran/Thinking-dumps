(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./data-directed.scm")

(load "./exercise_5_5_machine_state.scm")
(load "./exercise_5_5_handlers.scm")
(load "./exercise_5_5_simulator.scm")

;; instead of hand-simulation, let's try to write one simulator

(define fac-machine-controller
  '(controller
    (assign continue (label fact-done))
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
    after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
    base-case
    (assign val (const 1))
    (goto (reg continue))
    fact-done))

(define (fac-machine-state)
  (ms-reg-set
   'n 5
   (make-machine-with-insns
    (cdr fac-machine-controller))))

(ms-pretty-print (run-machine (fac-machine-state)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
