(define (transform-sexp exp)
  (out "transforming:")
  (pretty-print exp)
  (newline)
  ;; invariant:
  ;; * the inner-expressions are always transformed before
  ;;   its outer-expression
  ;; * the input is a valid s-exp
  ;;   and the output is a valid s-exp but without local definitions
  (cond
   ((or (self-evaluating? exp)
        (quoted? exp)
        (variable? exp))
    ;; forms that couldn't contain a sub-expression,
    ;; the transformation will just leave them unchanged
    exp)
   ((assignment? exp)
    ;; (set! <var> <exp>)
    `(set! ,(assignment-variable exp)
           ,(transform-sexp (assignment-value exp))))
   ((definition? exp)
    ;; We are not going to take two cases into account.
    ;; Instead, we "normalize" the definition so we are sure to
    ;; deal with a normalized form later
    ;; (this makes "lambda" the only form that the transformation cares about)
    (let ((exp (normalize-define exp)))
      ;; overwritten exp shadowing the original one
      `(define
         ,(definition-variable exp)
         ,(transform-sexp (definition-value exp)))))
   ((if? exp)
    ;; (if <pred> <cons> <alt>)
    ;; since the accessor assigns a value
    ;; when there is no alternative expression
    ;; the assumed syntax here is safe
    `(if ,(transform-sexp (if-predicate exp))
         ,(transform-sexp (if-consequent exp))
         ,(transform-sexp (if-alternative exp))))
   ((lambda? exp)
    ;; here we need to:
    ;; * scan exposed definitions
    ;; * eliminate them
    ;; we can do things in one traversal:
    ;; * scan definition, if something like "(define ...)"
    ;;   is found, change it to "(set! ...)" and put the variable
    ;;   somewhere
    ;;   this function will have type: SExp -> (Set Var, SExp)
    ;; * after this is done, wrap the subexpression with a "let"
    ;;   to include local variables
    (let* ((scan-result
            (scan-and-transform-exps (lambda-body exp)))
           (local-defs
            (car scan-result))
           (transformed-body
            (cdr scan-result))
           (transformed-body2
            `(let ,(map (lambda (var)
                          `(,var '*unassigned*))
                        local-defs)
               ,@(map transform-sexp transformed-body))))
      `(lambda ,(lambda-parameters exp)
         ,transformed-body2)))
   ((begin? exp)
    ;; (begin <exp> ...)
    `(begin ,@(map
               transform-sexp
               (begin-actions exp))))
   ((cond? exp)
    ;; well, let's desugar it
    (transform-sexp (cond->if exp)))
   ((let? exp)
    ;; well, let's desugar it
    (transform-sexp (let->combination exp)))
   ((application? exp)
    ;; (<exp1> <exp2s> ...)
    `(,(transform-sexp (operator exp))
      ,@(map transform-sexp (operands exp))))
   (else
    (error "invalid s-expression: "
           exp))))
