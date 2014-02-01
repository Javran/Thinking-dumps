(load "../common/utils.scm")
(load "../common/test-utils.scm")

; * If I remove binding from the first frame where
;     the variable is found, we cannot guaranteed that 
;     this variable is now not being occupied by anything else.
;     If the intension is to make any queries about that variable
;     cause errors, this method might not work.
; * If I remove all the occurences of a variable
;     along the path of enclosing-environment
;     this will lead to dangerous situation.
;     where we might end up removing some important
;     primitive bindings and there's no way to bring
;     these primitives back.
; * The best thing I can do is to add an extra field
;     into the environment to keep a list of variables
;     that has been `masked`. If the variable is listed
;     in the `marked` section, we just simply report
;     that the binding does not exist.

; the following code is based on
;   ./exercise_4_11.scm
; as I think it's more natural to think the structure
;   as an alist, rather than two lists of the same length.

(load "./my-eval.scm")

(load "./exercise_4_11_common.scm")

(define the-empty-environment nil)
(define empty-environment?    null?)
(define first-frame           car)
(define black-list            cadr)
(define enclosing-environment caddr)

(define (extend-environment vars vals base-env)
  (list (make-frame vars vals) '() base-env))

; check if `var` is blacklisted in the current frame
;   the callee should take the responsibility of recursive checking
(define (blacklisted? var env)
  (if (empty-environment? env)
    #f
    (memq var (black-list env))))

(define (remove-from-blacklist! var env)
  (set-car! (cdr env) (delq var (black-list env))))

(define (add-to-blacklist! var env)
  (set-car! (cdr env) (cons var (black-list env))))

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

(define (lookup-variable-value var env)
  (define (env-loop env)
    ; search a frame for the corresponding value
    (define (scan vars vals frame)
      (cond ((assq var (cdr frame)) => cdr)
            (else
              (env-loop (enclosing-environment env)))))
    (if (or (eq? env the-empty-environment)
            (blacklisted? var env))
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
    (if (or (eq? env the-empty-environment)
            (blacklisted? var env))
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
               (remove-from-blacklist! var env)
               (set-cdr! the-pair val) 
               'ok))
          (else
            (add-binding-to-frame! var val frame)))))

(test-environment
  lookup-variable-value
  extend-environment
  define-variable!
  set-variable-value!)
(out "Test passed.")

; ==== special form: (make-unbound! <var>)

(define (eval-make-unbound! exp env)
  (add-to-blacklist! (cadr exp) env))

(define (test-make-unbound!)
  (define env1
    (extend-environment
      (list 'a 'b 'c)
      (list 10 20 30)
      the-empty-environment))

  (define env2
    (extend-environment
      (list 'c)
      (list 40)
      env1))

  (assert-no-error
    (lambda ()
      (my-eval `b env2))
    "simple lookup")

  (my-eval `(make-unbound! c) env2)

  (assert-error
    (lambda ()
      (my-eval `c env2))
    "the variable has been blacklisted")

  (assert-no-error
    (lambda ()
      (my-eval `c env1))
    "blacklist should not have effects on enclosing environments")

  (my-eval `(define c 10) env2)

  (assert-no-error
    (lambda ()
      (my-eval `c env2))
    "the variable should be unblocked")

  'ok)


(define make-unbound!-handler
  (make-handler
    'make-unbound!
    eval-make-unbound!
    test-make-unbound!))

(handler-register! make-unbound!-handler)

;; force test to make sure this new env impl works
(test-my-apply)
(test-init-env)
(my-eval-test-installed-handlers)

(end-script)
