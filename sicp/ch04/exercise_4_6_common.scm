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

(define (test-let->combination let->combination)
  (define testcases
    (list
      (mat
        '(let ((x 1)
               (y 2)
               (z 3))
           (+ x y z))
        6)
      (mat
        '(let ((a 10)
               (b 20))
           (+ a a)
           (* b a))
        200)
      (mat
        '(let ()
           10)
        10)))
  (define (proc exp)
    (eval (let->combination exp)
          user-initial-environment))
  (do-test proc testcases))
