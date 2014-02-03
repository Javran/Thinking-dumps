(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./my-eval.scm")
(load "./exercise_4_11_common.scm")

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
              (if (eq? '*unassigned* (car vals))
                (error "variable not initialized:" var)
                (car vals)))
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

(test-environment
  lookup-variable-value
  extend-environment
  define-variable!
  set-variable-value!)

; test `'*unassigned*`
(let ((env (init-env)))
  (define-variable! 'a '*unassigned* env)
  (assert-error
    (lambda ()
      (lookup-variable-value 'a env))))

(end-script)
