(load-option 'synchronous-subprocess)

(load "../common/utils.scm")

; mit-scheme provides "run-shell-command" to run system commands, refer to:
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Subprocesses.html#Subprocesses

; if you encounter error like "Directory contains null component",
;     you might have multiple slashes in your $PATH, i.e.: "/etc/foo//bar"

(run-shell-command "ls -l"
  'output (current-output-port)
  'output-buffer-size 8192)

(let loop ((i 0))
  (if (< i 10)
    (begin
      (run-shell-command (string-append "echo commands from scheme! " (number->string i))
        'output (current-output-port)
        'output-buffer-size 8192)
    (loop (+ i 1)))))
