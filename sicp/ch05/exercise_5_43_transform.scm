(define (transform-exp exp)
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
           ,(transform-exp (assignment-value exp))))
   ((definition? exp)
    ;; We are not going to take two cases into account.
    ;; Instead, we "normalize" the definition so we are sure to
    ;; deal with a normalized form later
    ;; (this makes "lambda" the only form that the transformation cares about)
    (let ((exp (normalize-define exp)))
      ;; shadowing the original definition of exp
      `(define
         ,(definition-variable exp)
         ,(transform-exp (definition-value exp)))))
   ((if? exp)
    ;; (if <pred> <cons> <alt>)
    ;; since the accessor assigns a value
    ;; when there is no alternative expression
    ;; the assumed syntax here is safe
    `(if ,(transform-exp (if-predicate exp))
         ,(transform-exp (if-consequent exp))
         ,(transform-exp (if-alternative exp))))
   ((lambda? exp)
    ;; lambda-expression is the most important case
    ;; in this transformation, as it serves as the "scope boundary"
    ;; for local-variables. Lambda-expressions are allowed to
    ;; have local definitions, but these local definitions lie
    ;; in the scope of the lambda expression itself thus not lexically
    ;; accessible from outside.
    ;; Therefore we need to perform the following steps:
    ;;
    ;; 1. carefully scan the body of the lambda expression in question.
    ;;    this scan should extra all reachable local varaibles
    ;;    (by "reachable" I meant definitions not being inside of
    ;;    any inner let- or lambda- expressions)
    ;; 2. wrap the original lambda-body inside a let-binding,
    ;;    this outer let-binding is supposed to capture all local definitions
    ;;    found by step 1.
    ;; 3. transfrom the inner expression - those inner lexical scopes
    ;;    that we have ignored in step 1. (this violates the invariant we set
    ;;    at the openning comment of this function - but we can fix that)
    ;;
    ;; For better efficiency, we notice that when doing the local definition scan
    ;; we can already tell whether the expression in question forms a lexical scope.
    ;; if the expression does not, we do the transformation;
    ;; but nothing is done when the expression does form one - and we need a second scan
    ;; to find these expressions again and do the transformation for them.
    ;; Therefore, we can optimize this a little bit by allowing the local definition scanner
    ;; to do something on the expression:
    ;; when it hits a expression which forms a lexical scope,
    ;; we let the scanner to call "transform-exp" on its inner expressions.
    ;; Additionally, the scanner does one more extra thing: turns every local definition
    ;; into an assignement. As the local definitions will be captures by an outer
    ;; let-expression, this transformation is safe.
    ;; The benefits will be:
    ;;
    ;; * the invariant is preserved: we always transform the inner expression
    ;;   before the transformation of this outer exprssion is done.
    ;; * now we only need to traverse the expression once, but 3 things are done, namely:
    ;;   * local definitions are collected
    ;;   * at the same time, these definitions are also turned into assignments
    ;;   * inner expressions are local-definition-free transformed
    (let* ((scan-results
            (scan-and-transform-exps (lambda-body exp)))
           (binding-set (car scan-results))
           ;; the transformation is supposed to
           ;; eliminate all (direct) local definitions,
           ;; if this is true, then we can just transform these
           ;; subexpression directly.
           ;; by doing this, the recursive is guaranteed to run
           ;; on "smaller" structure thus will be able to terminate.
           ;;
           ;; NOTE: here we are doing mutual recursion: "transform-exp"
           ;; calls "scan-and-transform-exps" to collect local defitions
           ;; and get the transformed expression, whereas "scan-and-transform-exps"
           ;; calles "transform-exp" to help dealing with inner expression
           ;; transformations. Extreme care needs to be taken, as I spent
           ;; some time here figuring out why previously this was causing an infinite loop
           ;; and finally it turned out I called "transform-exp" on the resulting
           ;; expression returned by "scan-and-transform-exps".
           ;; This is not necessary, as the resulting expression is guaranteed to
           ;; be "local-definition-free". However, doing transformation on this
           ;; resulting expression can cause some trouble, because to eliminate
           ;; local definitions, let-expression might be introduced which desugars
           ;; again into lambda expressions, and we suddenly need to deal with yet
           ;; another layer of lexical scope -- as you can see, this never ends.
           (transformed-exps (cdr scan-results)))
      `(lambda ,(lambda-parameters exp)
         (let ,(map (lambda (var)
                      `(,var '*unassigned*))
                    binding-set)
           ,@transformed-exps))))
   ((begin? exp)
    ;; (begin <exp> ...)
    `(begin ,@(map
               transform-exp
               (begin-actions exp))))
   ((cond? exp)
    ;; well, let's desugar it
    (transform-exp (cond->if exp)))
   ((let? exp)
    ;; well, let's desugar it
    (transform-exp (let->combination exp)))
   ((application? exp)
    ;; (<exp> ...)
    (map transform-exp exp))
   (else
    (error "invalid s-expression: "
           exp))))
