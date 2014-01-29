(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_4_11_common.scm")

(define the-empty-environment nil)
(define empty-environment?    null?)
(define first-frame           car)
(define enclosing-environment cdr)

; a frame contains a list,
;   whose elements are name-value pairs
(define (make-frame variables values)
  (assert (= (length variables)
             (length values))
          "name-value list length mismatch") 
  ; simply zip two lists together
  ;   to make the pair
  (cons 'frame
        (map cons variables values)))

(define (frame-variables frame)
  (map car (cdr frame)))
(define (frame-values    frame)
  (map cdr (cdr frame)))

(define (add-binding-to-frame! var val frame)
  (set-cdr!  frame
    (cons
      ; the new pair
      (cons var val)
      ; existing bindings
      (cdr frame)))
  'ok)

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    ; search a frame for the corresponding value
    (define (scan vars vals frame)
      (cond ((assq var (cdr frame)) => cdr)
            (else
              (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
      (error "unbound variable" var)
      (let* ((frame (first-frame env))
             (vars (frame-variables frame))
             (vals (frame-values    frame)))
        (cond ((assq var (cdr frame)) => cdr)
              (else
                (env-loop (enclosing-environment env)))))))
  (env-loop env))

; pretty similiar to `lookup-variable-value`
(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "unbound variable" var)
      (let* ((frame (first-frame env))
             (vars (frame-variables frame))
             (vals (frame-values    frame)))
        (cond ((assq var (cdr frame)) =>
                 (lambda (the-pair)
                   (set-cdr! the-pair val)
                   'ok))
              (else
                (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (define-variable! var val env)
  ; seems the `env` requires to be a non-empty env
  (assert (not (empty-environment? env))
          "define-variable! cannot work on an empty environment")
  (let* ((frame (first-frame env))
         (vars (frame-variables frame))
         (vals (frame-values    frame)))
    (cond ((assq var (cdr frame)) =>
             (lambda (the-pair)
               (set-cdr! the-pair val) 'ok))
          (else
            (add-binding-to-frame! var val frame)))))

(test-environment
  lookup-variable-value
  extend-environment
  define-variable!
  set-variable-value!)
(out "Test passed.")

(end-script)
