(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")

(define (new-register)
  ;; the register stores the stack
  (let ((st (empty-stack)))
    ;; the stack must have
    ;; at least one element at any time
    (stack-push! st '*unassigned*)
    (vector st)))

(define (register-get reg)
  (stack-top (vector-ref reg 0)))

(define (register-set! reg val)
  (let* ((st (vector-ref reg 0)))
    (stack-pop! st)
    (stack-push! st val)
    (vector-set! reg 0 st)))

(let ((m (build-and-execute
          '(controller
            (assign a (const 10))
            (assign a (const 20))
            (assign a (const 30))
            (assign a (op +) (reg a) (reg a)))
          '())))
  (out (machine-reg-get m 'a)))


(end-script)
