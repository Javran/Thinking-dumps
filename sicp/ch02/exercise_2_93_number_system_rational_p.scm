(define (install-rational-p-package)

  (define make cons)

  (define numer car)
  (define denom cdr)

  (define (add-r x y)
    (make (add (mul (numer x) (denom y))
               (mul (numer y) (denom x)))
          (mul (denom x) (denom y))))
;   (define (neg-r x)
;     (make (mul (make-scheme-number -1)
;                (numer x))
;           (denom x)))
;   (define (sub-r x y)
;     (add-r x (neg-r y)))
;   (define (mul x y)
;     (make (mul (numer x) (numer y))
;           (mul (denom x) (denom y))))
;   (define (inv x)
;     (make (denom x) (numer x)))
;   (define (div x y)
;     (mul x (inv y)))
; 
  (define (=zero? x)
    (= (numer x) (make-scheme-number 0)))

  (define (equ? x y)
    (=zero? (sub x y)))

  (define (to-string-rp x)
    ; use square bracket instead of parentheses
    ;   to distinguish from polynomials
    (string-append
      "["
      (to-string (numer x))
      "/"
      (to-string (denom x))
      "]"
      ))

  (put 'make 'rational-p (tagged 'rational-p make))
  (put 'numer '(rational-p) numer)
  (put 'denom '(rational-p) denom)
  (put 'add '(rational-p rational-p) (tagged 'rational-p add-r))
;   (put 'sub '(rational-p rational-p) (tagged 'rational-p sub))
;   (put 'mul '(rational-p rational-p) (tagged 'rational-p mul))
;   (put 'div '(rational rational) (tagged 'rational div))
;   (put '=zero? '(rational) =zero?)
;   (put 'equ? '(rational rational) equ?)
  (put 'to-string '(rational-p) to-string-rp)
;   (put 'raise '(rational) raise)
;   (put 'test 'rational-package test-rational)
  'done)

