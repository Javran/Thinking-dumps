;; primitives

;; helper
(define (list-tagged-with tag)
  (lambda (l)
    (and
     (pair? l)
     (eq? (car l) tag))))

(define (tagged-list? exp tag)
  ((list-tagged-with tag) exp))

;; check if an expression is self-evaluating
;; i.e. (eval env exp) == exp
(define (self-evaluating? exp)
  ;; "()" shouldn't be self-evaluating:
  ;; in mit-scheme "'()" and "()" have no difference
  ;; but in guile "()" is considered a syntax error
  ;; however, "(quote ())" which is semantically the same as "'()"
  ;; seems to be the preferred way to represent the empty list
  (or (number? exp)
      (string? exp)
      (char? exp)
      (boolean? exp)))

;; if an given expression is a variable
(define variable? symbol?)

;; quoted expression
(define quoted?
  (list-tagged-with 'quote))
(define text-of-quotation cadr)

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define assignment-variable cadr)
(define assignment-value caddr)

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    ; form: (define <var> <value>)
    (cadr exp)
    ; `(cadr exp)` is a list
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    ;; first form
    (caddr exp)
    ;; second form, not supported here
    ;; however, exercise 5.23 provides a desugarizer
    ;; that transforms this second form into the first form
    ;; see "normalize-define" in "./exercise_5_23_trans.scm"
    (error "not supported")))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)

(define (if? exp)
  (tagged-list? exp 'if))
(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative exp)
  ; allowing the else part to be empty
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    #f))

(define (true? d)
  (if d #t #f))
(define (false? d)
  (if d #f #t))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define begin-actions cdr)
(define (last-exp? seq)
  ; `seq` must not be empty
  (null? (cdr seq)))
(define first-exp car)
(define rest-exps cdr)

(define (application? exp)
  (and (list? exp)
       (non-empty? exp)))
(define operator car)
(define operands cdr)

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (last-operand? ops)
  (null? (cdr ops)))

(load "ec-env.scm")

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define compound-procedure?
  (list-tagged-with 'procedure))
(define procedure-parameters  cadr)
(define procedure-body        caddr)
(define procedure-environment cadddr)

(define primitive-procedure?
  (list-tagged-with 'primitive))
(define primitive-implementation cadr)
;; lift a primitive procedure into
;; the implemented language
(define (lift-primitive prim)
  (list 'primitive prim))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define prompt-for-input display)
