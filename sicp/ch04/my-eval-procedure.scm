; require: make-begin
; procedure datatype
; data structure:
; * (list 'proc 'primitive <the proc>)
; * (list 'proc 'compound <args> <body> <env>)

(define (proc? proc)
  (and (> (length proc) 2)
       (eq? (car proc) 'proc)))

(define (proc-primitive? proc)
  (and (my-eval-proc? proc)
       (eq? (cadr proc) 'primitive)))

(define (proc-compound? proc)
  (and (my-eval-proc? proc)
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

(define (apply-proc-primitive proc args)
  (apply (car (proc-prim proc)) args))

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
