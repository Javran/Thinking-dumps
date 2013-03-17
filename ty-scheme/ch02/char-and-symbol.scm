(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(out #\c)
(out (char? #\c))
; #t
(out (char? 1))
; #f
(out (char? #\;))
; #t
(newline)

; char comparison
(out (char=? #\a #\a))
; #t
(out (char<? #\a #\b))
; 'a' < 'b' ?
; #t
(out (char>=? #\a #\b))
; #f
(newline)

; and their case-insensitive counterpart
(out (char-ci=? #\a #\A))
; #t
(out (char<? #\a #\B))
; #f
(out (char-ci<? #\a #\B))
; #t
(newline)

; upcase / downcase conversion
(out (char-downcase #\A))
(out (char-upcase #\b))
(newline)

; symbols
; 'self-evaluating' -> the evaluated result is what you've typed in
; like: #t, 42, #\c ...
; symbols behavior differs(they are identifiers for variables)
;     that evals to the value it holds
;     however: symbols are a simple data type

(out (quote xyz))
(out 'xyz)
;  xyz

(out (+ 1 2 3))
; 6
(out '(+ 1 2 3))
; evals to (+ 1 2 3) rather than 6

; checking symbolness
(out (symbol? 'xyz))
(out (symbol? 42))

; and symbols are case-insensitive
(out (eqv? 'XYZ 'xyz))

; define a global variable, use symbol 'xyz
(define xyz 9)
(out (symbol? xyz))
; now xyz evals to 9
; #f
(out xyz)

; use 'set!' to change value held
(set! xyz #\c)
(out xyz)
