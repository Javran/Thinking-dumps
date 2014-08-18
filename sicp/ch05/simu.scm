;; Let's try to create a better machine simulator
;; and name it "simu"

(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; toggle tests
(define *simu-test* #t)

(load "./simu_utils.scm")
(load "./simu_handlers.scm")
(load "./simu_accessors.scm")
(load "./simu_machine.scm")

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  ;; TODO: make alists instead of passing these arguments
  ;; which is error prone.
  ;; since we only run this once for each instruction,
  ;; I think the performance won't be an issue
  (let ((handler (get-handler (car inst))))
    (if handler
        (handler inst labels machine pc flag stack ops)
        (error "unknown instruction:" inst))))

;; TODO: not confident if the current system will be working,
;; try to at least make some handlers work.
(let* ((inst '(assign a (op +) (reg b) (const 10)))
       (labels '*not-used*)
       (machine (make-machine
                 ;; register names
                 '(a b)
                 ;; operations (not sure how to represent
                 'todo
                 ;; controller text
                 'todo))
       (handler (get-handler (car inst))))
  'ok)


;; Local variables:
;; proc-entry: ""
;; End:
