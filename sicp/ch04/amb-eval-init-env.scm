(load "./my-eval-init-env.scm")

;; initial environment
;; that provides some helper functions for amb-eval
(define (amb-init-env)
  (let ((env (init-env)))
    (amb-eval-all
     `(begin
        (define (require p)
          (if p
              'ok
              (amb)))

        (define (an-element-of items)
          (require (not (null? items)))
          (amb (car items)
               (an-element-of (cdr items))))

        (define (distinct? items)
          (cond ((null? items) #t)
                ((null? (cdr items)) #t)
                ((member (car items) (cdr items)) #f)
                (else (distinct? (cdr items)))))
        )
     env)
    env))
