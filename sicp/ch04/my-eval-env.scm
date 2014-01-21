; operations on environments
; * lookup-variable-value  :: Var x Env -> ExpVal
; * extend-environment     :: [Var] x [ExpVal] x Env -> Env
; * define-variable!       :: Var x ExpVal x Env -> Env
;     bind/modify a variable to a value in the current environment
;     the environment cannot be empty
;     if the variable is already contained in the current frame
;     it will bind to the new value instead
; * set-variable-value!    :: Var x ExpVal x Env -> Env
;     travel through frames and environments
;     find the first binding of a variable,
;     bind it to the new value

; data structure:
; an environment is either an empty environment,
;   or an object that contains a frame (first frame)
;   and an enclosing environment
; a frame can contain multiple variable - value bindings

(define the-empty-environment nil)
(define empty-environment?    null?)
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

(define (lookup-variable-value var env)
  ; travel through environments
  (define (env-loop env)
    ; search a frame for the corresponding value
    (define (scan vars vals)
      (cond ((null? vars)
              ; empty frame, next one
              (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
              ; symbol found
              (car vals))
            (else
              (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values    frame)))))
  (env-loop env))

; pretty similiar to `lookup-variable-value`
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
              (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
              (set-car! vals val))
            (else
              (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values    frame)))))
  (env-loop env))

(define (define-variable! var val env)
  ; seems the `env` requires to be a non-empty env
  (assert (not (empty-environment? env))
          "define-variable! cannot work on an empty environment")
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
              (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
              (set-car! vals val))
            (else
              (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values    frame))))

(define (test)
  (define env (extend-environment
                '(a b c)
                '(1 2 3)
                the-empty-environment))

  ; rebind some variables
  (define-variable! 'c 'cval env)
  (define-variable! 'b 'bval env)
  
  ; define new variable
  (define-variable! 'd 'd env)

  ; test lookup-variable-value
  (do-test
    lookup-variable-value
    (list
      (mat 'a env 1)
      (mat 'c env 'cval)
      (mat 'd env 'd)
      (mat 'b env 'bval)))

  ; new env
  (define env1 (extend-environment
                 '(c d e f)
                 '(10 20 30 40)
                 env))
  ; another new
  (define env2 (extend-environment
                 '(b)
                 '(bb22)
                 env))
  
  ; test lookup-variable-value in nested env structure
  (do-test
    lookup-variable-value
    (list
      ; from enclosing env
      (mat 'a env1 1)
      (mat 'b env1 'bval)
      ; shadowed
      (mat 'c env1 10)
      (mat 'd env1 20)
      ; newly created
      (mat 'e env1 30)
      (mat 'f env1 40)))

  ; define new variable
  (define-variable! 'g '50  env1)
  (define-variable! 'a 'aaa env1)

  ; modify existing
  (define-variable! 'c 'ccc env1)

  ; env shouldn't change, redo tests again on `env`
  (do-test
    lookup-variable-value
    (list
      (mat 'a env 1)
      (mat 'c env 'cval)
      (mat 'd env 'd)
      (mat 'b env 'bval)))

  ; new env1 test
  (do-test
    lookup-variable-value
    (list
      (mat 'a env1 'aaa)
      (mat 'b env1 'bval)
      (mat 'c env1 'ccc)
      (mat 'd env1 20)
      (mat 'e env1 30)
      (mat 'f env1 40)
      (mat 'g env1 50)))

  ; test set!
  (set-variable-value! 'b 'bbb env1)

  ; b's binding should all be changed
  (do-test
    lookup-variable-value
    (list
      (mat 'b env  'bbb)
      (mat 'b env1 'bbb)
      (mat 'b env2 'bb22)))

  ; set! on `env2` should not change bindings
  ;   in `env` and `env1` however.
  (set-variable-value! 'b 'new-b env2)

  (do-test
    lookup-variable-value
    (list
      (mat 'b env  'bbb)
      (mat 'b env1 'bbb)
      (mat 'b env2 'new-b)))
  
  'done)

(if *my-eval-do-test*
  (test))
