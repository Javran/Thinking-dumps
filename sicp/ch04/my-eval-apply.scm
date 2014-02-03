; procedure and application

; procedure datatype
; data structure:
; * (list 'proc 'primitive <the proc>)
; * (list 'proc 'compound <args> <body> <env>)

(define (proc? proc)
  (and (list? proc)
       (> (length proc) 2)
       (eq? (car proc) 'proc)))

(define (proc-primitive? proc)
  (and (proc? proc)
       (eq? (cadr proc) 'primitive)))

(define (proc-compound? proc)
  (and (proc? proc)
       (eq? (cadr proc) 'compound)))

(define (make-proc-primitive prim)
  (list 'proc 'primitive prim))

(define (make-procedure vars body env)
  (list 'proc 'compound vars body env))

; fetch the data fields of a procedure
(define proc-fields cddr)

; accessor for primitive procedures
(define proc-prim (compose car   proc-fields))

; accessor for compound procedures
(define proc-vars (compose car   proc-fields))
(define proc-body (compose cadr  proc-fields))
(define proc-env  (compose caddr proc-fields))

; application is just a non-empty list
(define (application? exp)
  (and (list? exp)
       (non-empty? exp)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-application rator-exp rand-exps)
  (cons rator-exp rand-exps))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (apply-proc-primitive proc args)
  (apply (proc-prim proc) args))

(define (apply-proc-compound proc args)
  (define vars (proc-vars proc))
  (define body (proc-body proc))
  (define env  (proc-env  proc))
  (my-eval
    ; by definition, the procedure body
    ;   is a sequence of expresssions
    (make-begin body)
    (extend-environment vars args env)))

; my-apply
(define (my-apply proc args)
  (define apply-proc
    (cond ((proc-primitive? proc)
            apply-proc-primitive)
          ((proc-compound? proc) 
            apply-proc-compound)
          (else
            (error
              "Unknown procedure type: APPLY" proc))))
  (apply-proc proc args))

(define (test-my-apply)
  (define env1
    the-empty-environment)

  (define proc+
    (make-proc-primitive +))

  (define proc-
    (make-proc-primitive -))

  (define proc*
    (make-proc-primitive *))

  (define proc-id
    (make-procedure
      ; args
      (list 'x)
      ; body
      (list 'x)
      env1))

  (define proc-branch
    (make-procedure
      ; args
      (list 'p 'a 'b)
      ;
      (list 
        (list 'if 'p 'a 'b))
      env1))

  (define testcases1
    (list 
      (mat proc+ '(1 2 3) 6)
      (mat proc- '(10 20) -10)
      (mat proc* '(1 4 9) 36)
      (mat proc-id '(#f) #f)
      (mat proc-branch '(#t 10 20) 10)
      (mat proc-branch '(#t 10 20) 10)
      ))

  ; test `my-apply` itself
  (do-test my-apply testcases1)

  (define env2
    (extend-environment
      ; only primitives, which do not require an env
      (list '+ '- '*)
      (list proc+ proc- proc*)
      env1))

  (define testcases2
    (list
      (mat '(+ 1 2 3 4) env2 10)
      (mat '(* (+ 1 2) (- 3 4)) env2 -3)
      (mat '(+ (* 1 2) (* 3 4)) env2 14)
      (mat '(- (* 3 4 5) (* 2 (- 7 2))) env2 50))) 

  ; test indirectly by calling `my-eval`
  (do-test my-eval testcases2)

  'ok)
