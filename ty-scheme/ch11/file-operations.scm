(load "../common/utils.scm")

(define test-file-name "test.txt")

(out (file-exists? test-file-name))
; should be #f, but it's ok if it returns #t

; output test file
(call-with-output-file test-file-name
  (lambda (f-port)
    (display "nice boat!" f-port)))

(out (file-exists? test-file-name))
; #t

; the equivalent in mit-scheme is called: file-attributes/modification-time
; refer to:
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/File-Manipulation.html#File-Manipulation
(display "Last modified: ")
(display (file-attributes/modification-time (file-attributes test-file-name)))
(newline)

(delete-file test-file-name)
(out (file-exists? test-file-name))
; #f
