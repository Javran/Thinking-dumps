(load "../common/utils.scm")

(define str-in (open-input-string "nice boat\na string port"))

(out (read-char str-in))
; #\n

(out (read-line str-in))
; "ice boat"

(out (read-line str-in))
; "a string port"

(define str-out (open-output-string))

(display "let's write something to a string" str-out)
(newline str-out)

; use 'get-output-string' to extract value inside the port
(out (get-output-string str-out))
