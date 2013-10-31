(load "../common/utils.scm")
(load "../common/test-utils.scm")

; TODO:
;   this exercise also needs boxes and lines
;   try to achieve it using LaTeX, in future.

(define (append! x y)
  (define (last-pair? x)
    (if (null? (cdr x)) x (last-pair (cdr x))))
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(out z)
; (a b c d)

(out (cdr x))
; (b)

(define w (append! x y))

(out w)
; (a b c d)

(out (cdr x))
; (b c d)

(end-script)
