(load "./simu_utils.scm")

;; operation expressions are of the form:
;; ((op <operator>) <operand1> <operand2> ...)
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (test-operation-exp-accessors)
  (let ((test1 '((op operator1) operand1 operand2))
        (test2 '((op opop) operand1)))
    (do-test
     operation-exp?
     (list
      (mat test1 #t)
      (mat test2 #t)
      (mat 'foo #f)
      (mat '((not-an-op x) aaa) #f)))
    (do-test
     operation-exp-op
     (list (mat test1 'operator1)
           (mat test2 'opop)))
    (do-test
     operation-exp-operands
     (list (mat test1 '(operand1 operand2))
           (mat test2 '(operand1))))
    'ok))

(if *simu-test*
    (begin
      (test-operation-exp-accessors))
    'skipped)

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:
