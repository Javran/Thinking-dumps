(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define def-env user-initial-environment)

(define true? identity)
(define true-value #t)
(define false-value #f)

; (and) => #t
; (and <expr>) => <expr>
; (and <expr1> ...) =>
; ((lambda (result)
;    (if result
;       (and ...)
;       #f))
;  (eval <expr> env))
(define (eval-and exp env)
  (define (and->if exp)
    (cond ((null? exp)
            ; this is something to be evaluated
            ;   as an expr
            ;   so I quote this value
            'true-value)
          ((null? (cdr exp))
            ; last expression, simply return it
            (car exp))
          (else
            (list
              (list 'lambda (list 'result)
                    (list 'if 'result
                          (list 'and (and->if (cdr exp)))
                          'false-value))
              (list 'eval (car exp) 'env)))))
  (eval (and->if (cdr exp))
        ; extend the default environment
        ;  to include the value of `env`
        (extend-top-level-environment
          env
          '(env)
          (list env))))

; (or) => #f
; (or <expr>) => <expr>
; (or <expr1> ...) =>
; ((lambda (result)
;    (if result
;       result
;       (or ...)))
;  (eval <expr> env))
(define (eval-or exp env)
  (define (or->if exp)
    (cond ((null? exp)
            'false-value)
          ((null? (cdr exp))
            (car exp))
          (else
            (list
              (list 'lambda (list 'result)
                    (list 'if 'result
                          'result
                          (list 'or (or->if (cdr exp)))))
              (list 'eval (car exp) 'env)))))
  (eval (or->if (cdr exp))
        (extend-top-level-environment
          env
          '(env)
          (list env))))
                       
(let ((testcases
        (list
          (mat '(and) #t)
          (mat '(and (= 1 1) (= 2 2)) #t)
          (mat '(and (= 1 1) #f (error 'wont-reach)) #f)))
      (proc
        (lambda (exp)
          (eval-and exp def-env))))
  (do-test proc testcases))

(let ((testcases
        (list
          (mat '(or) #f)
          (mat '(or #t (error 'wont-reach)) #t)
          (mat '(or (< 1 1) (> 2 2)) #f)))
      (proc
        (lambda (exp)
          (eval-or exp def-env))))
  (do-test proc testcases))

(end-script)
