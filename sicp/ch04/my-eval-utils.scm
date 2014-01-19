; find the value of `var` in `env`
; lookup-variable-value: Var x Env -> Value
(define (lookup-variable-value var env)
  (error "not implemented"))

; list-tagged-with: Tag -> List -> Bool
(define (list-tagged-with tag)
  (lambda (l)
    (and
      (list? l)
      (non-empty? l)
      (eq? (car l) tag))))

; same as `list-tagged-with` expect the argument
;   arrangement, to keep compatibility
; tagged-list?: List x Tag -> Bool
(define (tagged-list? exp tag)
  ((list-tagged-with tag) exp))

(define (test-utils)
  ; test list-tagged-with and tagged-list?
  (let ((testcases
          (list
            (mat 'a #f)
            (mat '(t1 a b) #t)
            (mat '(t2 b) #f)
            (mat '(t1 a b c) #t)
            (mat '() #f)))
        (proc (lambda (exp)
                (tagged-list? exp 't1))))
    (do-test (list-tagged-with 't1) testcases)
    (do-test proc testcases)))

(if *my-eval-do-test*
  (test-utils))
