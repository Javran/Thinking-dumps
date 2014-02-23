(define input-prompt "my-eval> ")
(define output-prompt "")

(define (driver-loop env)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input env)))
      (announce-output output-prompt)
      (user-print output)))
    (driver-loop env))

(define (prompt-for-input string)
  (format #t "~A" string))

(define (announce-output string)
  (format #t "~A" string))

(define (user-print object)
  (if (proc-compound? object)
    (out (list 'proc-compound
               (proc-vars object)
               (proc-body object)
               '<proc-env>))
    (out object)))

(define (my-eval-start-using-approach approach)
  (my-eval-select-approach approach)
  (driver-loop (init-env)))

(define (my-eval-start)
  (my-eval-start-using-approach 'analyze))
