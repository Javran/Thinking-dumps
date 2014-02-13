; not sure about
;   how we can introduce the change
;   from this section without doing too much change
;   let's just start writing a special form: analyze
;   the s-exp will look like: (analyze <exp>)
;   which does the analysis and returns a function
;   when the returned function is called with
;   suitable environment, that function gives the final answer.

(define (install-eval-analyze)
  (define (analyze-self-evaluating exp)
    (lambda (env)
      exp))

  (define (analyze-quoted exp)
    (lambda (env)
      (cadr exp)))
  
  (define (eval-analyze aexp env)
    (let ((exp (cadr aexp)))
      (cond ((self-evaluating? exp)
             (analyze-self-evaluating exp))
            ((quoted? exp)
             (analyze-quoted exp))
            ;; ((variable? exp)
            ;;  (analyze-variable exp))
            ;; ((assignment? exp)
            ;;  (analyze-assignment exp))
            ;; ((definition? exp)
            ;;  (analyze-definition exp))
            (else
             'todo))))

  (define (test)
    (define (analyze-and-go exp env)
      ((eval-analyze `(analyze ,exp) env) env))
    (define env (init-env))
    
    (do-test
     analyze-and-go
     (list
      (mat 1 env 1)
      (mat #t env #t)
      (mat #f env #f)
      (mat '(quote v) env 'v)
      (mat '(quote ((foo) bar)) env '((foo) bar))
      )
     equal?)
    'todo)
     
  (define handler
    (make-handler
     'analyze
     eval-analyze
     test))

  (handler-register! handler)
  'ok)
