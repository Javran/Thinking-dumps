(define (template-variable? sym)
  (and (symbol? sym)
       (eq? (string-ref
             (symbol->string sym)
             0)
            #\$)))

(define (template-match temp data env)
  (cond ((template-variable? temp)
         (let ((result (assoc temp env)))
           (if result
               (template-match (cadr result) data env)
               (cons (list temp data) env))))
        ((and (pair? temp)
              (pair? data))
         (let ((env1 (template-match (car temp)
                                     (car data)
                                     env)))
           (and env1
                (template-match (cdr temp)
                               (cdr data)
                               env1))))
        ((equal? temp data)
         env)
        (else #f)))

(display
 (template-match
  '($a $b ($a $c) $d)
  '(1 2 (1 (a c d)) good)
  '()))
(newline)
