; data structure:
;   handler is a list:
;     (list 'handler
;           <slot-eval>
;           <slot-analyze>
;           <test-proc>)
;   <slot-xxx>  : slot that this handler can deal with
;                 xxx = eval / analyze for `eval` / `analyze` modes
;   <test-proc> : a procedure that accepts zero argument
;                 do the handler self-test when called

(define (make-handler
          slot
          proc-eval
          proc-analyze
          test-proc)
  (list 'handler
        slot
        proc-eval
        proc-analyze
        test-proc))

(define (handler? h)
  (and (list? h)
       (not (null? h))
       (eq? (car h) 'handler)))

(define handler-slot cadr)
(define handler-proc-eval caddr)
(define handler-proc-analyze cadddr)
(define handler-test (compose car cddddr))

(define (handler-run-test h)
  (if (handler-test h)
    ((handler-test h))
    'no-test-available))

(define (handler-eval handler exp env)
  ((handler-proc-eval handler)
   exp
   env))

(define (handler-analyze handler exp)
  ((handler-proc-analyze handler)
   exp))

(define (handler-register! h)
  (my-eval-put! (handler-slot h) h))
