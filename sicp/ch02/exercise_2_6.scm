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
(out (map (lambda (c) ((c inc) 0))
          (list zero one two)))
; (0 1 2)

(end-script)
