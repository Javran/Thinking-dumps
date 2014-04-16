(load "./my-eval-driver-loop.scm")

(define input-prompt "amb-eval> ")
(define output-prompt "")

;; output amb informations, newline is appended
(define (amb-info fmt . args)
  (display "amb-eval: ")
  (apply format `(#t ,fmt ,@args))
  (newline))

(define (amb-repl-with env)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (amb-info "starting a new problem.")
            (amb-eval
             input
             env
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;; ambeval failure
             (lambda ()
               (amb-info "no more values of ~A" input)
               (amb-repl-with env)))))))
  (internal-loop
   (lambda ()
     (newline)
     (amb-info "no current problem")
     (amb-repl-with env))))

(define (amb-repl)
  (amb-repl-with (init-env)))

;; compatibility with definitions in the book
(define driver-loop amb-repl)
