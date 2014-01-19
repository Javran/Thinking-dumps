; data structure:
;   handler is a list:
;     (list 'handler
;           <slot>
;           <proc>
;           <test-proc>)
;   <slot>      : slot that this handler can deal with
;   <proc>      : the procedure of type Exp x Env -> ExpVal
;                 the handler itself
;   <test-proc> : a procedure that accepts zero argument
;                 do the handler self-test when called

(define (make-handler
          slot
          proc
          test-proc)
  (list 'handler
        slot
        proc
        test-proc))

(define (handler? h)
  (and (list? h)
       (not (null? h))
       (eq? (car h) 'handler)))

(define handler-slot cadr)
(define handler-proc caddr)
(define handler-test cadddr)

(define (handler-eval handler exp env)
  ((handler-proc handler)
   exp
   env))

(define (handler-register! h)
  (my-eval-put! (handler-slot h) h))
