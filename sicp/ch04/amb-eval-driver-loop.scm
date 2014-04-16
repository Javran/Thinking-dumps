(load "./my-eval-driver-loop.scm")

(define input-prompt "amb-eval> ")
(define output-prompt "")

;; output amb informations, newline is appended
(define (amb-info fmt . args)
  (display "amb-eval: ")
  (apply format `(#t ,fmt ,@args))
  (newline))

;; run REPL with a given environment
(define (amb-repl-with env)

  ;; called when user has issued "try-again"
  ;; without staring a problem
  (define on-failure-no-current-problem
    (lambda ()
      (amb-info "no current problem")
      (amb-repl-with env)))

  ;; called when user has issued "try-again"
  ;; but no more values are available
  (define (on-failure-no-more-values-of exp)
    (lambda ()
      (amb-info "no more values of ~A" exp)
      (amb-repl-with env)))

  ;; called when the computation has succeeded
  (define on-success
    (lambda (val next-alternative)
      (announce-output output-prompt)
      (user-print val)
      (internal-loop next-alternative)))

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
             on-success
             ;; ambeval failure
             (on-failure-no-more-values-of input)
             )))))
  (internal-loop on-failure-no-current-problem))

(define (amb-repl)
  (amb-repl-with (init-env)))

;; compatibility with definitions in the book
(define driver-loop amb-repl)
