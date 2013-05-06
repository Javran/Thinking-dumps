(load "../common/utils.scm")

; select iterative version of cont-frac
(define (cont-frac n d k)
  ; counter i from k to 1
  ; acc_next = N(i) / ( D(i) + acc ) 
  ; set acc to 0 initially
  (define (cont-frac-i i acc)
    (if (< i 1)
      acc
      (cont-frac-i (dec i) (/ (n i) (+ (d i) acc)))))
  (cont-frac-i k 0))

(define (d x)
  ; special case when i = 2, 5, 8 ... (i % 3 = 2)
  ; define j = 1,2,3 ... when i = 2,5,8 ...
  ; j = (i + 1) / 3
  (if (= (remainder x 3) 2)
    (* 2 (/ (inc x) 3))
    1))

; test d(i)
(out (map d
          (list-in-range 1 11)))
; 1 1 2 1 1 4 1 1 6 1 1 8

(define (calc-e k)
  (let ((e-2 (cont-frac (const 1.0) d k)))
    (+ 2 e-2)))

(let ((approx-e (calc-e 1000))
      (lib-e (exp 1)))
  (display "e from cont-frac:")
  (display approx-e)
  (newline)
  (display "e from scheme-lib:")
  (display lib-e)
  (newline)
  (display "difference:")
  (display (abs (- approx-e lib-e)))
  (newline))
