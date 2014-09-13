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
  (let ((st (vector-ref reg 0)))
    (stack-pop! st)
    (stack-push! st val)
    ;; "st" is itself stateful, don't have to
    ;; update the reference here
    ;; aka the following line is not necessary
    ;; (vector-set! reg 0 st)
    ))

(define (register-push! reg)
  (let* ((st (vector-ref reg 0))
         (t (stack-top st)))
    ;; to push is to duplicate the top element
    (stack-push! st t)))

(define (register-pop! reg)
  (let* ((st (vector-ref reg 0)))
    (stack-pop! st)
    ;; even we have one element, but
    ;; we won't be able to access the register
    ;; after the last element is popped from the stack
    ;; in order to have the same behavior
    ;; as what we have done for legacy simulator
    ;; I make the error happens earlier
    (if (stack-empty? st)
        (error "stack underflow")
        'ok)))



(let ((m (build-and-execute
          '(controller
            (assign a (const 10))
            (assign a (const 20))
            (assign a (const 30))
            (assign a (op +) (reg a) (reg a)))
          '())))
  (out (machine-reg-get m 'a)))


(end-script)
