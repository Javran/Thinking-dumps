(define input-prompt "amb-eval> ")
(define output-prompt "")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display "<Staring a new problem>")
            (amb-eval
             input
             the-global-environment
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-ouput output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-ouput
                (display "<no more values of>")
                (user-print input)
                (driver-loop))))))))
  (internal-loop
   (lambda ()
     (newline)
     (display "<no current problem>")
     (driver-loop))))
