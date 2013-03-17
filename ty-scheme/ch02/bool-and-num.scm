; summary:
; boolean?
; number?
; complex?
; real?
; rational?
; integer?

(display
  (boolean? #t))
; #t
(display
  (boolean? "nice"))
; #f

(display
  (not #f))
; #t

; anything other than #f is true
(display
  (not "hey"))
; #f

(newline)

; numbers
(display
  (number? 42))
; #t

(display
  (number? #t))
; #f

(display
  (complex? 2+3i))
; #t

(display
  (real? 2+3i))
; #f

(display
  (rational? 3.14))
; #t

(display
  (rational? 22/7))
; #t

(display
  (integer? 22))
; #t

(newline)

; base 2
(display
  #b100)
(newline)
; 4

; base 8
(display
  #o77)
(newline)
; 63

; base 16
(display
  #xffff)
(newline)
; 65535

; equality test
(display
  (eqv? 42 42))
; #t

(display
  (eqv? 42 #t))
; #f

(display
  (eqv? 42 42.0))
; #f
; note here eqv? fails
(newline)

; use = operator for numbers
(display
  (= 42 42.0))
; #t

(display
  (= 1.0 99/99))
; #t
(newline)

(display
  (< 3 2)) ; 3 < 2 ?
; #f

(display
  (< 1 2 3)) ; just like clojure, means test if 1<2<3
; #t

(display
  (>= 4.5 3))
; #t
(newline)

(display
  (+ 1 2 3))
; 6
(newline)

(display
  (- 5.3 2))
; 3.3
(newline)

(display
  (- 5 2 1)) ; 5-2-1=2
; 2
(newline)

(display
  (/ 24 1 2 3))
; 4
(newline)

(display
  (expt 2 3))
; 2^3 = 8
(newline)

(display
  (expt 4 1/2))
; 4^0.5 = 2
(newline)

(display
  (- 10))
; -10
(newline)

(display
  (/ 11/13))
; reciprocal: 13/11
(newline)

(define seq '(1 3 4 2 3))

(display
  (apply max seq))
(newline)
; 4

(display
  (apply min seq))
(newline)
; 1

(display
  (abs -4))
; 4
(newline)

(display
  (abs (- (- 2))))
; 2
(newline)
