#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(define (identifier? s)
  (and (symbol? s)
       (not (eq? s 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

(define (unparse-lc-exp-to-string exp)
  (cases lc-exp exp
    (var-exp (var)
      (symbol->string var))
    (lambda-exp (var body)
      (format "(lambda (~A) ~A)"
        var
        (unparse-lc-exp-to-string body)))
    (app-exp (rator rand)
      (format "(~A ~A)"
        (unparse-lc-exp-to-string rator)
        (unparse-lc-exp-to-string rand)))))

(define (parse-expression datum)
  (cond ((identifier? datum)
         (var-exp datum))
        ((not (list? datum))
          (eopl:error 'parse-expression
            "unexpected data, not a list: ~A" datum))
        ((null? datum)
          (eopl:error 'parse-expression
            "unexpected data, empty list"))
        ; datum must be a non-empty list starting from here
        ((eqv? (car datum) 'lambda)
          (assert (= (length datum) 3)
            "an lambda expression needs exactly 3 elements")
          (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum))))
        (else
          (assert (= (length datum) 2)
            "an application needs exactly 2 elements")
          (app-exp
             (parse-expression (car datum))
             (parse-expression (cadr datum))))))

(define (report-invalid-concrete-syntax datum)
  (eopl:error 'parse-expression
    "invalid concrete syntax: ~A" datum))

(define lc-exp1
  (parse-expression
    '((lambda (a) (a b)) c)))

(define lc-exp2
  (parse-expression
    '(lambda (x)
       (lambda (y)
         ((lambda (x)
            (x y))
         x)))))

(out (unparse-lc-exp-to-string lc-exp1)
     ; ((lambda (a) (a b)) c)
     (unparse-lc-exp-to-string lc-exp2)
     ; (lambda (x) (lambda (y) ((lambda (x) (x y)) x)))
     )

; error 1: wrong type
; (parse-expression 1)
; error 2: empty list
; (parse-expression '())
; errpr 3: call lambda with wrong length
; (parse-expression '(lambda (x) foo bar))
; error 4: application with wrong length
; (parse-expression '(a b c d))
