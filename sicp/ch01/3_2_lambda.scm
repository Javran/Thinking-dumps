(load "../common/utils.scm")

; actually I hate the parameter arrangemet here,
;     I would prefer "sum term next a b" ...
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
  
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(out (pi-sum 1 1000))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(out (integral cube 0 1 0.0001))
; ~= 0.25

; a lambda can be used any where a procedure can be used:
(out ((lambda (x y z) (+ x y (square z))) 1 2 3))
; 12

; let expression is interpreted as:
; ((lambda (<var1> ... <varn>)
;    <body>)
;   <expr1>
;   ...
;   <exprn>)
; * "let" allows one to bind variables *as locally as possible*
; * variables' value are computed outside the "let"
; 

(let ()
  (define x 2)
  (out (let ((x 3)
             (y (+ x 2)))
         (* x y))))
; the output will be "12" because y = x+2 is calculated outside the "let"
;     where x is "2" rather than "3"
