(define (template-variable? sym)
  (and (symbol? sym)
       (eq? (string-ref
             (symbol->string sym)
             0)
            #\$)))

(define (pattern-match pattern data env)
  (cond ((template-variable? pattern)
         (let ((result (assoc pattern env)))
           (if result
               (pattern-match (cadr result) data env)
               (cons (list pattern data) env))))
        ((and (pair? pattern)
              (pair? data))
         (let ((env1 (pattern-match
                      (car pattern)
                      (car data)
                      env)))
           (and env1
                (pattern-match
                 (cdr pattern)
                 (cdr data)
                 env1))))
        ((equal? pattern data)
         env)
        (else #f)))

(define (template-apply temp env)
  (cond ((template-variable? temp)
         (let ((result (assoc temp env)))
           (if result
               (cadr result)
               (error "unbound variable:"
                      temp))))
        ((pair? temp)
         (cons (template-apply (car temp) env)
               (template-apply (cdr temp) env)))
        (else temp)))

(display
 (template-apply
  '($a $c ($a $b))
  '(($a 100)
    ($b 200))))

(newline)
