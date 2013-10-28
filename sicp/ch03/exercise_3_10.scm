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

; (define (make-withdraw inital-amount)
;   ((lambda (balance)
;      (lambda (amount)
;        (if (>= balance amount)
;          (begin
;            (set! balance (- balance amount))
;            balance)
;          "Insufficient funds")))
;    inital-amount))
 
; 1. by defining `make-withdraw`:
;   global env (G) += make-withdraw
;   value that `make-withdraw` points to:
;     a pair:
;       parameters: inital-amount
;       body:
;       (lambda (inital-amount)
;         ((lambda (balance)
;            (lambda (amount)
;              (if (>= balance amount)
;                (begin
;                  (set! balance (- balance amount))
;                  balance)
;                "Insufficient funds")))
;          inital-amount))
 
(define W1 (make-withdraw 100))

; 2. evaluate `(make-withdraw 100)` and bind the result to `W1` (in G)
;   new env (E1) += inital-amount, inital-amount = 100
;   now evaluate the following expr in E1:
;         ((lambda (balance)
;            (lambda (amount)
;              (if (>= balance amount)
;                (begin
;                  (set! balance (- balance amount))
;                  balance)
;                "Insufficient funds")))
;          inital-amount)  
;   new env (E2) += balance, balance = 100
;   now evaluate the following expr in E2:
;            (lambda (amount)
;              (if (>= balance amount)
;                (begin
;                  (set! balance (- balance amount))
;                  balance)
;                "Insufficient funds"))
;   result: a procedure in environment: [E2, E1, G]

; verify my conclusion
(pp W1)
; the code analyzed above should show up

; 3. evaluate `(W1 50)`
;   new env (E3) += amount, amount = 50
;              (if (>= balance amount)
;                (begin
;                  (set! balance (- balance amount))
;                  balance)
;                "Insufficient funds")
;   balance (E2) = 100
;              (if #t
;                (begin
;                  (set! balance (- balance amount))
;                  balance)
;                _)
;   furthermore:
;                (begin
;                  (set! balance 50)
;                  balance)
;   no binding for `balance` in E3
;   binding `balance` firstly founded in E2.
;   bind value `50` to `balance` in E2
;   return `balance`, which is now `50`
(out (W1 50))
; 50






(end-script)
