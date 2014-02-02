(load "../common/utils.scm")
(load "../common/test-utils.scm")

; most of the stuffs have been done
;   in `my-eval` implementation.

; see `./my-eval-env.scm`

(define (setup-environment)
  (let ((initial-env
          (extend-environment
            (primitive-procedure-names)
            (primitive-procedure-objects))))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-procedure-names)
  (map car primitive-procedures))

; a primitive procedure is:
; a list with first element being the symbol
;   `primitive`, followed by the actual implementation
;   as second element
(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-implementation proc)
  (cadr proc))

; use the underlying `apply` to do the trick
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(end-script)
