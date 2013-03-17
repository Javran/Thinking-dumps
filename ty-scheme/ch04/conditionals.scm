(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(define p 80)

(out (if (> p 70)
  'safe
  'unsafe))

(out (if (< p 90)
       'low-pressure))

; else branch is optional
(out (if (> p 90)
       'low-pressure))

; #!unspecific <- what is this?

; when and unless

;(when #t
;  (out "open valve")
;  (out "attach floor-pump tube")
;  (out "depress floor-pump")
;  (out "detach floor-pump tube")
;  (out "close valve"))

; (unless #f
;  (out "open valve")
;  (out "attach floor-pump tube")
;  (out "depress floor-pump")
;  (out "detach floor-pump tube")
;  (out "close valve"))

; seems 'when' and 'unless' are not defined in mit-scheme

; cond
(define c #\a)
(out
  (if (char<? c #\c)
    -1
    (if (char=? c #\c)
      0
      1)))

(out
  (cond
    ((char<? c #\c) -1)
    ((char=? c #\c) 0)
    (else 1)))

(cond
  (#t
   (out "cond actions")
   (out "are implicit beginS")))

; case - a special case of the cond
(define case-test
  (lambda (c)
    (case c
      ((#\a) 1)
      ((#\b) 2)
      ((#\c) 3)
      (else 4))))

(out (case-test #\a))
; 1
(out (case-test #\f))
; 4

; and, or

; when #f is met in argument of an 'and' the remaining things will not be evaled
; when #t is met in argument of an 'or' the remaining things will not be evaled
(out (and 1 2))
; 2
(out (and 1 'a 'b #f 'c 'd))
; #f
(out (or #f 'a 'b 'c))
; a
