(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(out (char->integer #\d))
; 100
(out (integer->char 65))
; A

(out (string->list "hello"))
; breaks into a list of char

(out (list->string '(#\n #\i #\c #\e)))
; "nice"

(out (vector->list #(1 2 3)))
; (1 2 3)
(out (list->vector '(1 2 3)))
; #(1 2 3)

(out (number->string 16))
; "16"

(out (list->vector (string->list (number->string 12345))))
; #(1 2 3 4 5)

(out (string->number "Am I a hot number?"))
; conversion might fail
; returns '#f'

(out (string->number "16" 8))
; optional radix: 8
; 14

(out (symbol->string 'symbol))
; "symbol"

(out (string->symbol "string"))
; string (a symbol)
