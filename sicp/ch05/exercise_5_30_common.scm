(load "./exercise_5_30_error.scm")

(define (lookup-variable-value var env)
  ;; traverse through environments
  (define (env-loop env)
    ;; search a frame for the corresponding value
    (define (scan vars vals)
      (cond ((null? vars)
             ;; empty frame, next one
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             ;; symbol found
             (car vals))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (make-error 'unbound-variable var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values    frame)))))
  (env-loop env))

;; pretty similiar to `lookup-variable-value`
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
        (make-error 'unbound-variable var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values    frame)))))
  (env-loop env))
