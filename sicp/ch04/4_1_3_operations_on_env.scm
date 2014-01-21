(load "../common/utils.scm")
(load "../common/test-utils.scm")

; operations on environments
; * lookup-variable-value
;   :: Var x Env -> ExpVal
; * extend-environment
;   :: [Var] x [ExpVal] x Env -> Env
; * define-variable!
;   :: Var x ExpVal x Env -> Env
; * set-variable-value!
;   :: Var x ExpVal x Env -> Env

; data structure:
; an environment is either an empty environment,
;   or an object that contains a frame (first frame)
;   and an enclosing environment

(define the-empty-environment nil)
(define first-frame           car)
(define enclosing-environment cdr)

; a frame contains a list of bindings
(define (make-frame variables values)
  (cons variables values))
(define frame-variables car)
(define frame-values    cdr)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))
  'ok)

(define (extend-environment vars vals base-env)
  (assert (= (length vars)
             (length vals))
          "Variable-Value list length mismatch")
  (cons (make-frame vars vals) base-env))

(end-script)
