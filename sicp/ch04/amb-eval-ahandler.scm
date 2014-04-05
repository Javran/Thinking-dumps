;; amb-handler: callback handlers for amb evaluator
;; representation:
;; (list 'amb-handler
;;       <slot-name>
;;       <slot-analyze>
;;       <slot-test>)
;; <slot-name> :: Symbol
;;   a single symbol indicating the special form it handles
;; <slot-analyze> :: Sexp -> (Env -> Value)
;;   the slot handler, the whole expression will be passed
;;   to this procedure, in return this procedure should produce
;;   another procedure that accepts an environment and produces
;;   the final answer.
;; <slot-test> :: () -> Result
;;   the slot to test the handler. When this slot is called,
;;   the corresponding handler should be installed and ready to use
;;   this callback procedure can be either `#f` or a procedure that accepts
;;   no arguments. If you choose to provide a test, make sure you return a symbol
;;   "ok" to indicate that test procedure doesn't think there is any problem.

(define (make-amb-handler
         slot-name
         proc-analyze
         proc-test)
  (list 'amb-handler
        slot-name
        proc-analyze
        proc-test))

(define (ahandler? h)
  (and (list? h)
       (not (null? h))
       (eq? (car h) 'amb-handler)))

(define ahandler-slot         cadr)
(define ahandler-proc-analyze caddr)
(define ahandler-test         cadddr)

(define (ahandler-run-test h)
  (let ((tester (ahandler-test h)))
    (if tester
        (tester)
        'no-test-available)))

(define (ahandler-analyze handler exp)
  ((ahandler-proc-analyze handler)
   exp))

(define (ahandler-register! h)
  (my-eval-put! (ahandler-slot h) h))
