(load "../common/utils.scm")
(load "../common/test-utils.scm")

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
    (if (empty-environment? env)
      (error "unbound variable" var)
      ; search a frame for the corresponding value
      (let ((result (find-in-frame var (first-frame env))))
        (if result
          ; symbol found
          (car result)
          ; empty frame, next one
          (env-loop (enclosing-environment env))))))
  (env-loop env))

; pretty similiar to `lookup-variable-value`
(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (empty-environment? env)
      (error "unbound variable" var)
      (let ((result (find-in-frame var (first-frame env))))
        (if result
          (set-car! result val)
          (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  ; seems the `env` requires to be a non-empty env
  (assert (not (empty-environment? env))
          "define-variable! cannot work on an empty environment")
  (let ((frame (first-frame env)))
    (let ((result (find-in-frame var frame)))
      (if result
        ; the binding exists, overwrite it
        (set-car! result val)
        ; new to add new binding
        (add-binding-to-frame! var val frame)))))

; find `var` in `frame`
;   return #f if not found,
;   elsewise, return the partial list with first value
;   being the corresponding value of `var`.
;   works much like an `assq`
;   XXX: notice the return value is NOT A COPY
;     which means it can be modified by the callee
;     after this function returned
(define (find-in-frame var frame)
  (define (scan vars vals)
    (cond ((null? vars)
            #f)
          ((eq? var (car vars))
            vals)
          (else
            (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
        (frame-values    frame)))

(test-environment
  lookup-variable-value
  extend-environment
  define-variable!
  set-variable-value!)
(out "Test passed.")

(end-script)
