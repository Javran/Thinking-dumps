(load "../common/utils.scm")

; symbols and values
(define a 1)
(define b 2)

(out (list a b))
; (1 2)
(out (list 'a 'b))
; (a b)
(out (list 'a b))
; (a 2)

(out (car '(a b c)))
; a
(out (cdr '(a b c)))
; (b c)

; eq? - test if two symbols are the same
(out (eq? 'a 'a))
; #t
(out (eq? 'a 'b))
; #f
(out (eq? 1 2))
; #f

(define (my-memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (my-memq item (cdr x)))))

(newline)
(out (my-memq 'a '(pear banana prune)))
; #f
(out (my-memq 'a '(x (a sauce) y a pear)))
; (a pear)

(end-script)
