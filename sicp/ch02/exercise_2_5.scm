(load "../common/utils.scm")

(define (cons-i a b)
  (* (expt 2 a) (expt 3 b)))

; removes factor a from n
; and returns how many time a is removed from n
(define (rm-factor a n)
  (define (rm-iter rest acc)
    (if (= (remainder rest a) 0)
      (rm-iter (/ rest a) (+ acc 1))
      acc))
  (rm-iter n 0))

(define (car-i n)
  (rm-factor 2 n))

(define (cdr-i n)
  (rm-factor 3 n))

(define t (cons-i 123 456))

(out (car-i t)
     (cdr-i t))
; 123
; 456

(end-script)
