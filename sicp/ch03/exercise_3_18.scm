(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (contains-cycle? x)
  (define (contains-cycle-aux? x visited)
    (cond  ((memq x visited) #t)
           ((pair? x)
             (contains-cycle-aux? (cdr x) (cons x visited)))
           (else #f)))
  (contains-cycle-aux? x nil))


(end-script)
