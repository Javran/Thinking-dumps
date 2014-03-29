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
;   finally, bind `W1` to global env G

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

(define W2 (make-withdraw 100))

; 4. evaluate `(make-withdraw 100)` and bind the result to `W2` (in G)
;   new env (E4) += inital-amount, inital-amount = 100
;   now evaluate the following expr in E4:
;         ((lambda (balance)
;            (lambda (amount)
;              (if (>= balance amount)
;                (begin
;                  (set! balance (- balance amount))
;                  balance)
;                "Insufficient funds")))
;          inital-amount)  
;   new env (E5) += balance, balance = 100
;   now evaluate the following expr in E5:
;            (lambda (amount)
;              (if (>= balance amount)
;                (begin
;                  (set! balance (- balance amount))
;                  balance)
;                "Insufficient funds"))
;   result: a procedure in environment: [E5, E4, G]
;   finally, bind `W2` to global env G
(pp W2)
; code here should be the same as that of W1's
;   however, their environments are totally different:
;   [E2,E1,G] for W1, [E5,E4,G] for W2


; comparing `make-withdraw` in section 3.2.3
; (define (make-withdraw balance)
;   (lambda (amount)
;     (if (>= balance amount)
;       (begin (set! balance (- balance amount))
;              balance)
;       "Insufficient funds")))
; the difference is that `make-withdraw` in this exercise
;   has one more level of `let`, so the version in this exercise
;   has one more level of environment.

#|
instead of drawing pictures, I'll use some other syntax to represent things:
  an environment is:
    a set of bindings, and its parent environment, G = global environment
  a function is:
    a function body, parameter, and its environment, function body omitted

#1: after `make-withdraw` is defined

G: parent= nil
  make-withdraw -> func: para= (inital-amount), env= G

#2: after `W1` is set

G: parent= nil
  make-withdraw -> ...
  W1 -> func: para= (amount), env= E2
E1: parent= G
  inital-amount -> 100
E2: parent= E1
  balance -> 100

#3: `(W1 50)`

G: parent= nil
  make-withdraw -> ...
  W1 -> func: para= (amount), env= E2
E1: parent= G
  inital-amount -> 100
E2: parent= E1
  balance -> 50
E3(orphan): parent= E2
  amount -> 50

#4: after `W2` is set

G: parent= nil
  make-withdraw -> ...
  W1 -> func: para= (amount), env= E2
E1: parent= G
  inital-amount -> 100
E2: parent= E1
  balance -> 50
E3(orphan): parent= E2
  amount -> 50
E4: parent= G
  inital-amount -> 100
E5: parent= E4
  balance -> 100

|#

(end-script)
