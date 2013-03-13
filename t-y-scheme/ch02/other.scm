(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(out cons)
; a procedure

(out display)
; another procedure

(out out)
; a user-defined procedure

(out (current-output-port))
; a port

(display "Port!" (current-output-port))
; display with explicit port
