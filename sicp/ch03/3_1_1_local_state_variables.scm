(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define balance)

(set! balance 100)

(define (withdraw amount)
  (if (< balance amount)
    (out "Insufficient funds")
    (begin
      (set! balance (- balance amount))
      (out balance))))

(for-each withdraw '(25 25 60 15))

(newline)
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               (out balance))
        (out "Insufficient funds")))))
(for-each new-withdraw '(25 25 60 15))


(end-script)
