;; [SExp] -> (Set Var, [SExp])
(define (scan-and-transform-exps exps)
    (let* ((scan-results
            (map scan-definitions-and-transform exps))
           (result-sets
            (map car scan-results))
           (transformed-exps
            (map cdr scan-results)))
      (cons (fold-right set-union '() result-sets)
            transformed-exps)))

;; SExp -> (Set Var, SExp)
(define (scan-definitions-and-transform exp)
  (cond
   ((or (self-evaluating? exp)
        (quoted? exp)
        (variable? exp))
    ;; no new definition, keep original expression
    (cons '() exp))
   ((assignment? exp)
    ;; (set! <var> <exp>)
    (let ((scan-result (scan-definitions-and-transform
                        (assignment-value exp))))
      ;; pass inner definitions, create transformed expression
      (cons (car scan-result)
            `(set! ,(assignment-variable exp)
                   ,(cdr scan-result)))))
   ((definition? exp)
    (let ((exp (normalize-define exp)))
      ;; one local definition detected
      (let ((scan-result (scan-definitions-and-transform
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
          (transform-sexp exp)))
   ((begin? exp)
    (let ((scan-result
           (scan-and-transform-exps (begin-actions exp))))
      (cons (car scan-result)
            `(begin ,@(cdr scan-result)))))
   ((cond? exp)
    ;; desugar it
    (scan-definitions-and-transform (cond->if exp)))
   ((let? exp)
    ;; desugar it
    (scan-definitions-and-transform (let->combination exp)))
   ((application? exp)
    (scan-and-transform-exps exp))
   (else
    (error "invalid s-expression: "
           exp))))
