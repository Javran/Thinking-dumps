(define (remove-duplicates xs)
  (if (null? xs)
      '()
      (cons (car xs)
            (delete
             (car xs)
             (remove-duplicates (cdr xs))))))

;; if two sets are equal
;; (order is not taken into account)
(define (set-equal? s1a s2a)
  (let ((s1 (remove-duplicates s1a))
        (s2 (remove-duplicates s2a)))
    (cond ((null? s1) (null? s2))
          ((null? s2) (null? s1))
          (else
           (set-equal?
            (cdr s1)
            (delete (car s1) s2))))))

;; TODO: refactor
(do-test
 set-equal?
 (list
  (mat '(1 2 3) '(3 2 1) #t)
  (mat '(1 (a b (c (d))))
       '((a b (c (d))) 1) #t)
  (mat '() '(1 2) #f)
  (mat '(1 2) '() #f)))

(define (result-frame-equal? r1 r2)
  (or (and (eq? r1 'failed)
           (eq? r2 'failed))
      (set-equal? r1 r2)))

;; to "instantiate" an expression is
;; to replace variables with their values
(define (instantiate-exp exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 ;; call handler if the value cannot be found
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp))
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
