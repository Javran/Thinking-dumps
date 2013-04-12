#!/usr/bin/env guile

!#

(load "cgi.scm")

(define on-error #f)

; ensure the operator is valid
(define ensure-operator
  (lambda (e)
    ; if allowed, return the operator
    (case e
      ((+) +)
      ((-) -)
      ((*) *)
      ((/) /)
      ((**) expt)
      (else (on-error "unpermitted operator")))))

; ensure number is vaild
(define ensure-number
  (lambda (e)
    (if (number? e) 
      ; return the number
      e
      ; failed
      (on-error "non-number"))))

(define calc-eval
  (lambda (expr)
    (if (pair? expr)
      ; expression
      (apply
        ; ensure the operator
        (ensure-operator (car expr))
        ; recursively ensure every argument
        (map calc-eval (cdr expr)))
      ; else return the number itself
      (ensure-number expr))))

(define print-page-begin
  (lambda ()
    (display "content-type: text/html\r\n\r\n")
    (display "<html><head><title>A Scheme Calculator</title></head>")
    (display "<body>")))

(define print-page-end
  (lambda ()
    (display "</body></html>")))


(define print-form
  (lambda ()
    (display "<form action=\"")
    (display (getenv "SCRIPT_NAME"))
    (display "\">")
   
    (display "Enter arithmetic expression:<br/>")
    (display "<input type=textarea name=arithexp><p>")
    (display "<input type=submit value=\"Evaluate\">")
    (display "<input type=reset value=\"Clear\">")
    (display "</form>")))

(define print-example
  (lambda (expr)
    (display "<li>")
    (display expr)
    (display "</li>")))

(define print-examples
  (lambda ()
    (display "<ul>")
    (for-each print-example
              '(
                "1234"
                "(+ 1 2 3 4 5)"
                "(* 1 2 3 4 5 6 7 8 9 10)"
                "(* 5 (- 5 (/ 1 5)))"
                "(** 2 64)"
                ))
    (display "</ul>")))

(parse-form-data)

(print-page-begin)
(print-form)

(let ((exprs (form-data-get "arithexp")))
  (unless (null? exprs)
    ; get first element as the expression to be eval-ed
    (let ((expr (car exprs)))
      (display-html expr)
      (display "<p>=&gt;&nbsp;&nbsp;")
      ; prints "=>  "
      (display-html
        ; resume #1
        (call/cc
          (lambda (k)
            ; set on error callback
            (set! on-error
              ; on-error should be call with a reason 's'
              (lambda (s)
                (k (string-append "Error: " s))))
            (number->string
              (calc-eval (read (open-input-string expr)))))))
      (display "<p>"))))

(display "<h2>Try test cases:</h2>")
; TODO: item -> links, hint: need url encoding
(print-examples)

(print-page-end)
