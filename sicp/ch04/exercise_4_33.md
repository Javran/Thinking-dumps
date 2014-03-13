I won\'t implement this exercise, because it will introduce more confusions
than the problem it will solve. It costs too much to implement lazy evluation
as a native support rather than an extension, and the behavior of an lazy evaluator
might disagree with eager ones. I insist on implementing `stream` and using `list->stream`
to do the trick rather than using things like `'(a b c)` which semantically suggests
a normal list.

Edit: Here I provide some pseudo-code for completeness.

```scheme
;; test if the expression is a quoted list
(define (quoted-list? exp)
  (and (eq? 'quote (car exp))
       (list? (cdr exp))))

(define (list->lazy-list ls env)
  ;; we don't need to use `delay` here
  ;; because the evaluator is already lazy by default.
  ;; we deconstruct using implementing language's `cons`
  ;; and reconstruct using implemented language's `cons`
  (fold-right
    (lambda (x acc)
      (my-eval `(cons ,x ,acc) env))
    '() ls))

(define (eval-quote exp env)
  ;; this special form looks like:
  ;; (quote <...>)
  (cond ((quoted-list? exp)
         ;; convert to lazy list
         (let ((elements
                (map (lambda (x)
                     ;; evaluate each element
                     ;; and preserve the quotation
                     ;; in order to handle nested lists
                       (eval-quote `(quote ,x) env))
                     (cdr exp))))
           (list->lazy-list elements env)))
        (else
          (handle-other-cases exp env))))
```
