(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_4_4_common.scm")

; (and) => #t
; (and <expr>) => <expr>
; (and <expr1> ...) =>
; ((lambda (result)
;    (if result
;       (and ...)
;       #f))
;  <expr1>)
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
            (let ((result-sym (gensym)))
              (list
                (list 'lambda (list result-sym)
                      (list 'if result-sym
                            (list 'and (and->if (cdr exp)))
                            'false-value))
                (car exp))))))
  (eval (and->if (cdr exp)) env))

; (or) => #f
; (or <expr>) => <expr>
; (or <expr1> ...) =>
; ((lambda (result)
;    (if result
;       result
;       (or ...)))
;  <expr1>)
(define (eval-or exp env)
  (define (or->if exp)
    (cond ((null? exp)
            'false-value)
          ((null? (cdr exp))
            (car exp))
          (else
            (let ((result-sym (gensym)))
              (list
                (list 'lambda (list result-sym)
                      (list 'if result-sym
                            result-sym
                            (list 'or (or->if (cdr exp)))))
                (car exp))))))
  (eval (or->if (cdr exp)) env))

(test-and-or eval-and eval-or)

(end-script)
