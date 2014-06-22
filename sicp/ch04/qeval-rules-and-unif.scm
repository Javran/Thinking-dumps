;; dependencies:
;; - qeval-stream
;; - qeval-base
;; - qeval-transform
;; - qeval-database
;; - qeval-frames

;; whether an expression proposed to be the value
;; of a pattern variable depends on the variable.
;; in other words, is "var" involved in the expression part?
;; TODO: find some testcases
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               ;; if the variable is exactly the expression,
               ;; then yes
               #t
               (let ((b (binding-in-frame e frame)))
                 (if b
                     ;; the expression is a variable
                     ;; and it has a binding in the frame,
                     ;; we examine it recursively
                     (tree-walk (binding-value b))
                     #f))))
          ;; recursively run on the structure
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else #f)))
  (tree-walk exp))

;; try unifying two patterns and extend the existing frame
;; if this is not possible, return a symbol "failed"
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-consistent p1 p2 frame))
        ((var? p2) (extend-if-consistent p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           ;; the variable has been bound to a value,
           ;; try unifying with that value
           (unify-match
            (binding-value binding) val frame))
          ;; "var" is not bound to anything
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 ;; but "val" is bound to something in the frame
                 ;; in this case, we want to bind "var" with the binding value
                 (unify-match
                  var (binding-value binding) frame)
                 ;; otherwise, neither "var" nor "val" is bound to anything,
                 ;; we extend the existing frame
                 (extend var val frame))))
          ((depends-on? val var frame)
           ;; there isn't a general method to solve the problem like:
           ;; find a "?y" such that "?y" is equal to the expression
           ;; involving "?y". We reject this kind of expression
           ;; even sometimes it is possible and meaningful
           'failed)
          (else (extend var val frame)))))

;; change variables in a rule to unique names
;; to prevent the variables from different rule applications
;; from becoming confused with each other
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable
              exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (apply-a-rule rule query-pattern query-frame)
  ;; rename variables and work on the renamed one
  (let ((clean-rule (rename-variables-in rule)))
    ;; try to do the unification
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          ;; the unification has failed,
          ;; an empty stream is returned since no valid solution
          ;; is available
          the-empty-stream
          ;; if it is possible to unify the pattern with the conclusion,
          ;; we need to further check if the rule body makes sense
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

;; TODO: need some testcases somehow
(define (apply-rules pattern frame)
  (stream-intermap
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
