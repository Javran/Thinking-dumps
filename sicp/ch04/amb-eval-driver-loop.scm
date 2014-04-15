(load "./my-eval-driver-loop.scm")

(define input-prompt "amb-eval> ")
(define output-prompt "")

;; output amb informations, newline is appended
(define (amb-info fmt . args)
  (display "amb-eval: ")
  (apply format `(#t ,fmt ,@args))
  (newline))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (amb-info "starting a new problem.")
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
               (amb-info "no more values of ~A" input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (amb-info "no current problem")
     (driver-loop))))

;; TODO: amb-repl and amb-repl-with <env>
