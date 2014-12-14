;; expression transformers
;; most of them are copied from my previous exercises
;; do a code base search would find them

(define (make-if predicate consequent alternative)
  `(if ,predicate
       ,consequent
       ,alternative))

(define (make-begin exp-seq)
  `(begin
     ,@exp-seq))

(define (make-lambda parameters body)
  `(lambda ,parameters
     ,@body))

(define (last-exp? seq)
  (null? (cdr seq)))

(define first-exp car)
(define rest-exps cdr)

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define cond-clauses cdr)

(define cond-predicate car)
(define cond-actions cdr)

(define (clause-arrow? clause)
  (eq? (cadr clause) '=>))

(define clause-handler caddr)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        ;; no case is given
        'false
        (let ((first (car clauses))
              (rest  (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  ;; else part ... convert the seq to exp
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              ;; ((lambda (result)
              ;;    (if result
              ;;      <action>
              ;;      ...))
              ;;  <predicate>)
              (let ((result-sym (gensym)))
                ;; make an application
                (list
                 ;; operator
                 (make-lambda
                  ;; parameters
                  (list result-sym)
                  ;; body
                  (list
                   (make-if
                    result-sym          ; use cached result
                    (if (clause-arrow? first)
                        ;; the extended syntax
                        ;;   should be an application
                        (list
                         ;; operator
                         (clause-handler first)
                         ;; operand
                         result-sym)
                        ;; the original syntax
                        (sequence->exp (cond-actions first)))
                    (expand-clauses rest))))
                 ;; operand
                 (cond-predicate first)))))))
  (expand-clauses (cond-clauses exp)))

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

      ; (let <proc> <bindings> <body>)
      ; =>
      ; (let ()
      ;   (define (<proc> <binding-vars>)
      ;     <body>)
      ;   (<proc> <binding-exps>))
      (define (named-let->combination exp)
        (define proc-name (cadr exp))
        (define let-binding-pairs (caddr exp))
        (define let-body (cdddr exp))
        (define vars (map car  let-binding-pairs))
        (define exps (map cadr let-binding-pairs))
        (normal-let->combination
          (list 'let '()
                (cons 'define
                      (cons (cons proc-name vars)
                            let-body))
                (cons proc-name exps))))

      (if (named-let? exp)
        (named-let->combination exp)
        (normal-let->combination exp)))
