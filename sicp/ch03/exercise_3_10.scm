(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (make-withdraw inital-amount)
  (let ((balance inital-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))))

; recall the syntactic sugar:
; (let ((<var> <exp>)) <body>)
; => ((lambda (<var>) <body>) <exp>)

; rewrite `make-withdraw` to desugar `let`

(define (make-withdraw inital-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
         (begin
           (set! balance (- balance amount))
           balance)
         "Insufficient funds")))
   inital-amount))

; 1. by defining `make-withdraw`:
;   global env (G) += make-withdraw
;   value that `make-withdraw` points to:
;     a pair:  

(end-script)
