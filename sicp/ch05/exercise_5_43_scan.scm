;; this module includes "scan and tranform" functionality

;; scans all accessible
;; locally-defined variables from the expression
;; and transforms inner subexpressions (using transform-exp)
;; into their local-definition-free forms
;; SExp -> (Set Var, SExp)
(define (scan-and-transform-exp exp)
  (cond
   ((or (self-evaluating? exp)
        (quoted? exp)
        (variable? exp))
    ;; no new definition, keep original expression
    (cons '() exp))
   ((assignment? exp)
    ;; (set! <var> <exp>)
    (let ((scan-result (scan-and-transform-exp
                        (assignment-value exp))))
      ;; pass inner definitions, create transformed expression
      (cons (car scan-result)
            `(set! ,(assignment-variable exp)
                   ,(cdr scan-result)))))
   ((definition? exp)
    (let ((exp (normalize-define exp)))
      ;; one local definition detected
      (let ((scan-result (scan-and-transform-exp
                          (definition-value exp))))
        (cons (set-insert (definition-variable exp)
                          (car scan-result))
              ;; definition translated into assignment
              `(set! ,(definition-variable exp)
                     ,(cdr scan-result))))))
   ((if? exp)
    ;; (if <pred> <cons> <alt>)
    ;; since the accessor assigns a value
    ;; when there is no alternative expression
    ;; the assumed syntax here is safe
    (let ((scan-result
           ;; (cdr exp) => (<pred> <cons> <alt>)
           (scan-and-transform-exps (cdr exp))))
      (cons (car scan-result)
            `(if ,@(cdr scan-result)))))
   ((lambda? exp)
    ;; this is the tricky part: lambda is the definition boundary
    ;; we will perform the transformation inside, but passing
    ;; the empty set of defintions out.
    ;; this loop can be terminated
    ;; because this lambda-expression will be structurally smaller
    (cons '()
          (transform-exp exp)))
   ((begin? exp)
    (let ((scan-result
           (scan-and-transform-exps (begin-actions exp))))
      (cons (car scan-result)
            `(begin ,@(cdr scan-result)))))
   ((cond? exp)
    ;; desugar it
    (scan-and-transform-exp (cond->if exp)))
   ((let? exp)
    ;; desugar it
    (scan-and-transform-exp (let->combination exp)))
   ((application? exp)
    (scan-and-transform-exps exp))
   (else
    (error "invalid s-expression: "
           exp))))

;; the same as "scan-and-transform-exp", but for a list
;; of expressions, local variable sets are unioned together.
;; [SExp] -> (Set Var, [SExp])
(define (scan-and-transform-exps exps)
    (let* ((scan-results
            (map scan-and-transform-exp exps))
           (result-sets
            (map car scan-results))
           (transformed-exps
            (map cdr scan-results)))
      (cons (fold-right set-union '() result-sets)
            transformed-exps)))
