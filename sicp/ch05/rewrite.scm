;; a template system that recognizes patterns in data and replaces
;; them using redefined rules (templates)

;; see notes in "5_3_2_maintaining_the_illusion_of_infinite_memory.md"
;; for related discussions

;; a template variable is a symbol
;; that begins with "$"
(define (template-variable? sym)
  (and (symbol? sym)
       (eq? (string-ref
             (symbol->string sym)
             0)
            #\$)))

;; pattern match against data
;; with know variable-value bindings in env
;; values found in "env" shouldn't contain any
;; template variable
;; returns the expanded environment (variable-value bindings)
;; if the pattern match fails, #f is returned
(define (pattern-match pattern data env)
  (cond ((template-variable? pattern)
         ;; check if the variable has a known binding
         (let ((result (assoc pattern env)))
           (if result
               ;; see if varaible binding and data agree
               (pattern-match (cadr result) data env)
               ;; if a binding isn't present, bind this variable
               (cons (list pattern data) env))))
        ((and (pair? pattern)
              (pair? data))
         ;; do pattern matching recursively
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

;; apply a template using bindings in env
;; all template variables should be available
;; in `env`, and values in `env` shouldn't contain
;; template variables
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

;; try to rewrite data using rules
;; a rule is of the following form:
;; (list <pattern> <template>)
;; return the modified data or #f if
;; none of these rules apply.
;; this procedure will only try to use the first rule
;; that applies.
(define (try-rewrite-once rules data)
  (fold-left
   (lambda (modified rule)
     (or modified
         (let* ((pat (car rule))
                (temp (cadr rule))
                (env (pattern-match pat data '())))
           (and env
                (template-apply temp env)))))
   #f
   rules))
