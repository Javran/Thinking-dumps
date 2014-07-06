;; query syntax transformation between internal representation
;; and the representation from user input / for visualization

(define (query-syntax-process exp)
  ;; search symbols recursively and apply "proc" to it
  (define (map-over-symbols proc exp)
    (cond ((non-empty? exp)
           (cons (map-over-symbols proc (car exp))
                 (map-over-symbols proc (cdr exp))))
          ((symbol? exp)
           (proc exp))
          (else exp)))

  (define (expand-question-mark symbol)
    (let ((chars (symbol->string symbol)))
      (if (string=? (substring chars 0 1) "?")
          (list '?
                (string->symbol
                 (substring chars 1 (string-length chars))))
          symbol)))
  (map-over-symbols expand-question-mark exp))

;; a variable is of form (? <var>) or (? <num> <var>)
(define var?
  (list-tagged-with '?))
;; a constant symbol is a scheme symbol
(define constant-symbol? symbol?)

;; unique variables constructed during rule application
;; is generated using the following code

;; generate a new rule application id,
;; I think it's better to have "!" at the end
;; to indicate that this procedure has side effects
(define new-rule-application-id!
  (let ((rule-counter 0))
    (lambda ()
      (set! rule-counter (add1 rule-counter))
      rule-counter)))

;; the generated variable is of form "(? <num> <var>)"
(define (make-new-variable var rule-application-id)
  `(? ,rule-application-id ,@(cdr var)))

;; convert back the transformed expression
(define (contract-question-mark variable)
  (string->symbol
   (string-append
    "?"
    (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
        (symbol->string (cadr variable))))))

(define (test-qeval-syntax-trans)
  (define (test-query-syntax-process)
    (do-test
     query-syntax-process
     (list
      (mat '(job ?x ?y) '(job (? x) (? y)))
      (mat '?symbol '(? symbol))
      (mat '(job x y) '(job x y)))))

  (define (test-make-new-variable)
    (do-test
     make-new-variable
     (list
      (mat '(? var) 1 '(? 1 var))
      (mat '(? sym) 123 '(? 123 sym)))))

  (define (test-contract-question-mark)
    (do-test
     contract-question-mark
     (list
      (mat '(? 10 var) '?var-10)
      (mat '(? 1 one) '?one-1)
      (mat '(? var) '?var))))

  (test-query-syntax-process)
  (test-make-new-variable)
  (test-contract-question-mark)
  'ok)

(if *qeval-tests*
    (test-qeval-syntax-trans)
    'ok)

;; local variables:
;; proc-entry: "./qeval.scm"
;; End:
