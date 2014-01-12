#lang eopl

(require "./common.rkt")
(require "./test-utils.rkt")

(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)))

(define (parse-prefix-exp datum)
  ; parse an exp, return a pair: ( parsing result . elements left )
  (define (parse-exp datum)
    (cond ((or (not (list? datum))
               (null? datum))
            (eopl:error 'parse-prefix-exp
              "the expression is invalid or incomplete"))
          ((eqv? (car datum) '-)
            (let* ((result1 (parse-minus datum))
                   (result2 (parse-exp (cdr result1)))
                   (result3 (parse-exp (cdr result2))))
              (cons
                (diff-exp (car result2) (car result3))
                (cdr result3))))
          ((integer? (car datum))
            (cons
              (const-exp (car datum))
              (cdr datum)))
          (else
            (eopl:error 'parse-prefix-exp
              "invalid syntax: ~A" datum))))
  ; parse a minus operator
  (define (parse-minus datum)
    (if (and (list? datum)
             (not (null? datum))
             (eqv? (car datum) '-))
      (cons '- (cdr datum))
      (eopl:error 'parse-prefix-exp
        "not a minus operator: ~A" datum)))
  (car (parse-exp datum)))

(define (unparse-prefix-exp exp)
  (cases prefix-exp exp
    (const-exp (n)
      (list 'const-exp n))
    (diff-exp (rand1 rand2)
      (list 'diff-exp
            (unparse-prefix-exp rand1)
            (unparse-prefix-exp rand2)))))

(out (unparse-prefix-exp (parse-prefix-exp '(- - 3 2 - 4 - 12 7))))
