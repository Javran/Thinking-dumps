(load "./my-eval-driver-loop.scm")

(define input-prompt "amb-eval> ")
(define output-prompt "")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (out "amb-eval: starting a new problem.")
            (amb-eval
             input
             (init-env)
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-output "amb-eval: no more values of ")
                (user-print input)
                (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (out "amb-eval: no current problem")
     (driver-loop))))
