; please use Guile for this source file
(load "../common/guile/utils.scm")

; capture signal
(sigaction SIGALRM
           (lambda (sig)
             (display "Signal ")
             (display sig)
             (display " raised. Continuing...")
             (newline)))

; test alarm
(alarm 1)

; use read-char to prevent quiting
(read-char)
