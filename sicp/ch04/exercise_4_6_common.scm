; (let ((<var1> <exp1>)
;       (<var2> <exp2>)
;       ...
;       )
;   <body>)
; =>
; ((lambda (<var1> <var2> ...)
;    <body>)
;  <exp1>
;  <exp2>
;  ...
;  )
(define (let->combination exp)
  (define let-binding-pairs (cadr exp))
  (define let-body (cddr exp))
  (define vars (map car  let-binding-pairs))
  (define exps (map cadr let-binding-pairs))
  (cons
    ; operator
    (make-lambda vars let-body)
    ; operands
    exps))

(define (let? exp)
  (tagged-list? 'let exp))

(define (eval-let exp env)
  (eval (let->combination exp) env))
