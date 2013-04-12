(load "../common/utils.scm")

; here x is a global variable
(define x 9)

(define add2
  (lambda (x)
    ; the x above is only used locally
    (+ x 2)))

(out 9)
; 9
(out (add2 3))
; 5
(out x)
; 9


; set! modifies variable binding of a visible symbol
(set! x 10)
(out x)
; 10
(define add2-mod
  (lambda (x)
    (set! x 0)
    ; only the local x is affected
    ;     which is the only 'x' that the 'set!' above can see
    (+ x 2)))
(out (add2-mod x))
; will always be 2
(out x)
; and the binding of global x keeps unchanged - 10

(define counter 0)
(define bump-counter
  (lambda ()
    (set! counter (+ counter 1))
    counter))

; bump-counter is a zero-argument function that cannot shallow any variable
;     it will inc counter and return the current value of it

(out (bump-counter))
(out (bump-counter))
(out (bump-counter))
; 1, 2, 3

; but what if we make a function that shallows counter?
(define var-shallow-test
  (lambda ()
    (define counter 0)
    (out counter)
    (bump-counter)
    (out counter)))

(out counter)
; 3
(var-shallow-test)
; 0
; 0
; nothing changed during execution ... why?
;     IMHO that is because 'bump-counter' can see no more variables
;     than its definition (lambda) can see.
;     check the example 'var-shallow-test2' below
; update: please read the text again:
; "In any text, an identifier named x refers to the lexically closest variable named x"
;     and of course the 'bump-counter' defined above knows only the one defined right before it
;     (i.e. "lexically closest")

(out counter)
; 4

(define var-shallow-test2
  (lambda ()
    (define counter 0)
    (define bump-counter
      (lambda ()
	(set! counter (+ counter 1))
	counter))
    (out (bump-counter))
    (out (bump-counter))))

(var-shallow-test2)
; 1
; 2
(out counter)
; 4 - well, the counter keep unchanged
