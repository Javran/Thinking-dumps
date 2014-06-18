;; transform variables to make it easier for processing

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

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

;; deal with the expression after transformation

;; a variable is of form (? <var>)
(define var?
  (list-tagged-with '?))
;; a constant symbol is a scheme symbol
(define constant-symbol? symbol?)

;; unique variables constructed during rule application
;; is generated using the following code

;; global rule counter to ensure the uniqueness
(define rule-counter 0)
;; generate a new rule application id,
;; I think it's better to have "!" at the end
;; to indicate that this procedure has side effects
(define (new-rule-application-id!)
  (set! rule-counter (add1 rule-counter))
  rule-counter)

;; TODO: not sure about what's the structure of "var"
;; seems like "var" should be of form "(? <var>)"
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

(define (test-qeval-transform)
  (define (test-expand-question-mark)
    (do-test
     expand-question-mark
     (list
      (mat '?test '(? test))
      (mat 'whatever 'whatever)
      (mat '?symbol '(? symbol)))))

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

  (test-expand-question-mark)
  (test-query-syntax-process)
  (test-make-new-variable)
  (test-contract-question-mark)
  'ok)

(if *qeval-tests*
    (test-qeval-transform)
    'ok)

;; local variables:
;; proc-entry: "./qeval.scm"
;; End:
