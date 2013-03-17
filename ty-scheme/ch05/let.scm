(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(define x 20)
(let (
      (x 1)
      (y 2)
      (z 3))
  ; the global x gets masked
  (out (list x y z)))
; (1 2 3)

(let (
      (y 2)
      (z 3))
  ; this time x is not masked
  (out (list x y z)))
; (20 2 )

(let (
      (x 1)
      (y x))
  (out (+ x y)))
; x is bound to 1
; y is bound to x - but not the x defined right below it
; so 'let' just define things parallelly
; output is 21 because x = 1, y = 20

; to introduce lexical variables in sequence, we have 'let*'

(let* (
       (x 1)
       (y x))
  (out (+ x y)))
; now the output is 2

; let* is entirely equivalent to nested let below:
(let ((x 1))
  (let ((y x))
    (out (+ x y))))
; the output is 2
; as we've expected, nested-lets force definition to go in sequence

; variables can be procedures as well
(let ((cons (lambda (x y)
	      (+ x y))))
  ; 'cons' gets shallowed inside this scope
  (out (cons 1 2)))
; 3
(out (cons 1 '(2 3 4)))
; cons is not affected outside
