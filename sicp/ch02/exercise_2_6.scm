(load "../common/utils.scm")

(define zero
  (lambda (f)
    (lambda (x) x)))

(define add-1
  (lambda (n)
    (lambda (f)
      (lambda (x) (f ((n f) x))))))

; one is:
; (add-1 zero)
; (add-1 (lambda (f) (lambda (x) x)))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; (lambda (f) (lambda (x) (f (             (lambda (x) x)     x))))
; (lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

; two is:
; (add-1 one)
; (add-1 (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f (f x))))
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

; to verify it ... we can apply "inc" and "0"
;     to "convert" a Church numerals into a number:
(define (to-num c) ((c inc) 0))

(out (map to-num
          (list zero one two)))
; (0 1 2)

; define "add" without the help of "add-1" ...
; to begin with, I'd like to see what will (add-1 (add-1 n)) become:
; now I'll express "lambda" as "\"
; add-1 = \n.\f.\x. f (n f x)
; add-1 n = \f.\x. f (n f x)
; add-1 (add-1 n) = \f.\x. f ((\f.\x. f (n f x)) f x)
;                 = \f.\x. f (        f (n f x)     )
;                 = \f.\x. f (f (n f x))
; add-2 = \f.\x. f (f (n f x))
; so add-2 is just applying f again on the output of add-1
; given that:
; "zero" when applied with anything will become a "identity function"
; "one" applies x to f
; "two" applies the result of "one" to f  ...
; we can come up with add:
; add-m n = \f.\x. f (f (f ... (f (n f x)) ...))
;                  ^  f*m times ^
; replace "f" with "f1"
; replace "n f x" with "x1"
; we have:
; add-m n = \f.\x. (\f1.\x1. f1 (f1 (f1 ... (f1 x1) ...))) f (n f x)
; add-m n = \f.\x. m f (n f x)
; to sum up:
; add = \m.\n.\f.\x. m f (n f x)

(define add
  (lambda (m n)
    (lambda (f)
      (lambda (x)
        ((m f) ((n f) x))))))

(out (to-num (add one two)))
; 3



(end-script)
