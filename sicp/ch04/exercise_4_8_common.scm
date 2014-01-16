(define (let->combination exp)
  (define (named-let? exp)
    ; the second element
    ;   should be a symbol
    ;   for named-let
    (symbol? (cadr exp)))

  (define (normal-let->combination exp)
    (define let-binding-pairs (cadr exp))
    (define let-body (cddr exp))
    (define vars (map car  let-binding-pairs))
    (define exps (map cadr let-binding-pairs))

    (cons
      ; operator
      (make-lambda vars let-body)
      ; operands
      exps))

  ; (let <var> <bindings> <body>)
  ; =>
  ; (let ((<var>
  ;         (lambda <vars-in-bindings>
  ;           <body>)))
  ;   (<var> <exps-in-bindings))
  (define (named-let->combination exp)
    (define proc-name (cadr exp))
    (define let-binding-pairs (caddr exp))
    (define let-body (cdddr exp))
    (define vars (map car  let-binding-pairs))
    (define exps (map cadr let-binding-pairs))

    (define constructed-let
      (list
        'let 
        (list (list proc-name
                    (make-lambda
                      vars
                      let-body)))
        ; application
        (cons proc-name exps)))
    (out constructed-let)
    (normal-let->combination constructed-let))

  (if (named-let? exp)
    (named-let->combination exp)
    (normal-let->combination exp)))

