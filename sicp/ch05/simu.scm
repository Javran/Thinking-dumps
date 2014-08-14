;; Let's try to create a better machine simulator
;; and name it "simu"

(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; toggle tests
(define *simu-test* #t)

(load "./simu_handlers.scm")
(load "./simu_accessors.scm")

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

;; Local variables:
;; proc-entry: ""
;; End:
