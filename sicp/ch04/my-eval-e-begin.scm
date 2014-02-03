; eval begin-exp

; make a sequence of expressions
;   into an expression of `(begin ...)`
(define (make-begin exp-seq)
  (cons 'begin exp-seq))

; test if `seq` is a sequence with exactly one exp
(define (last-exp? seq)
  ; `seq` must not be empty
  (null? (cdr seq)))

(define first-exp car)
(define rest-exps cdr)

; turn a seq of exps into a begin-exp
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (install-eval-begin)

  (define begin-actions cdr)


  (define (eval-sequence exps env)
    (cond ((null? exps)
            ; let's allow empty seq
            ;   and the return value is simply false
            '#f)
          ((last-exp? exps)
            ; the last expression
            ;   in addition to the evaluation
            ;   we should return this value as well
            (my-eval (first-exp exps) env))
          (else
            ; `(last-exp? exps)` also guarantees that the first case
            ; (i.e. `(null? exps)`) can never be reached by recursive calls
            ; so if something has fallen into this case,
            ; then we are dealing with some non-empty seq
            ; with more than one element
            (my-eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))

  (define (test)
    (define env
      (extend-environment
        (list 'a 'b 'c)
        (list 1 2 3)
        the-empty-environment))

    (define testcases
      (list
        (mat '(begin) env #f)
        (mat '(begin a) env 1)
        (mat '(begin 1 2 3) env 3)
        (mat '(begin a b b b c c c) env 3)
        (mat '(begin 30 (if #t 10 20)) env 10)
        (mat '(begin 30 (if #f 10 20)) env 20)
        (mat '(begin (if #t 10 20) 30) env 30)
        (mat '(begin (if #f 10 20) 30) env 30)
        ))
    (do-test eval-begin testcases)
    'ok)

  (define handler
    (make-handler
      'begin
      eval-begin
      test))

  (handler-register! handler)

  'ok)
