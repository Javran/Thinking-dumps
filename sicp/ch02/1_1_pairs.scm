(load "../common/utils.scm")

(let ((x (cons 1 2)))
  (out (car x)
       (cdr x)))
; 1
; 2

(let* ((x (cons 1 2))
       (y (cons 3 4))
       (z (cons x y)))
  (out (car (car z))
       (car (cdr z))))
; 1
; 3

; representing rational numbers
(define make-rat cons)
(define numer car)
(define demon cdr)

; I'd like to name it display-rat rather than print-rat
(define (display-rat x)
  (display (numer x))
  (display "/")
  (display (demon x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (demon y))
               (* (numer y) (demon x)))
            (* (demon x) (demon y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (demon y))
               (* (numer y) (demon x)))
            (* (demon x) (demon y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (demon x) (demon y))))

(define (div-rat x y)
  (make-rat (* (numer x) (demon y))
            (* (demon x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (demon y))
     (* (numer y) (demon x))))

(define one-half (make-rat 1 2))
(display-rat one-half)
(newline)

(define one-third (make-rat 1 3))
(display-rat (add-rat one-half one-third))
(newline)
; 1/2 + 1/3 = 5/6

(display-rat (sub-rat one-half one-third))
(newline)
; 1/2 - 1/3 = 1/6

(display-rat (mul-rat one-half one-third))
(newline)
; 1/2 * 1/3 = 1/6

(display-rat (div-rat one-half one-third))
(newline)
; 1/2 / 1/3 = 3/2

(display-rat (add-rat one-third one-third))
(newline)
; 1/3 + 1/3 = 2/3
; actually prints 6/9 here.

; remedy this by changing make-rat:
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

(display-rat (add-rat one-third one-third))
; produces 2/3 now

(end-script)
