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
        'false
        (let ((first (car clauses))
              (rest  (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              (let ((result-sym (gensym)))
                (list
                 (make-lambda
                  (list result-sym)
                  (list
                   (make-if
                    result-sym
                    (if (clause-arrow? first)
                        (list
                         (clause-handler first)
                         result-sym)
                        (sequence->exp (cond-actions first)))
                    (expand-clauses rest))))
                 (cond-predicate first)))))))
  (expand-clauses (cond-clauses exp)))

(define (let->combination exp)
  (define (named-let? exp)
    (symbol? (cadr exp)))

  (define (normal-let->combination exp)
    (define let-binding-pairs (cadr exp))
    (define let-body (cddr exp))
    (define vars (map car  let-binding-pairs))
    (define exps (map cadr let-binding-pairs))

    `(,(make-lambda vars let-body)
      ,@exps))

  (define (named-let->combination exp)
    (define proc-name (cadr exp))
    (define let-binding-pairs (caddr exp))
    (define let-body (cdddr exp))
    (define vars (map car  let-binding-pairs))
    (define exps (map cadr let-binding-pairs))
    (normal-let->combination
     ;; TODO: quasiquote?
     (list 'let '()
           (cons 'define
                 (cons (cons proc-name vars)
                       let-body))
           (cons proc-name exps))))

  (if (named-let? exp)
      (named-let->combination exp)
      (normal-let->combination exp)))

;; Local variables:
;; proc-entry: "./exercise_5_23.scm"
;; End:
