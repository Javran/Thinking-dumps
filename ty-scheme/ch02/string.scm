(load "../common/utils.scm")

; strings evaluate to themselves as well
(out "Nice boat!")

; 'string' joins chars together
(out (string 
       #\v #\e #\r #\y
       #\space 
       #\n #\i #\c #\e
       #\space
       #\b #\o #\a #\t #\!))

(define greeting "0123456789")

(out (string-ref greeting 1))
; 1
(out (string-ref greeting 8))
; 8

; concat strings

(out (string-append 
       "E "
       "Pluribus "
       "Unum"))

; make a string with specified length
; we can fill it with desired char later
(define a-3-char-long-string (make-string 3))

(out (string? #t))
; #f
(out (string? #\t))
; #f
(out (string? (string #\t)))
; #t

(string-set! a-3-char-long-string 0 #\h)
(string-set! a-3-char-long-string 1 #\e)
(string-set! a-3-char-long-string 2 #\y)
(out a-3-char-long-string)
; hey
