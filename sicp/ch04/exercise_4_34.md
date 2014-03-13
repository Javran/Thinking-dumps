I think a `promise` representation is enough,
just as the other scheme implementation does.
We should keep `print` only print the information
the evaluator knows rather than force  some more values
that causes side effects and potential failure.

Edit: I provide some pseudo-code without tests, just for completeness.

```scheme
;; this function should be called when
;; a lazy-pair needs to print out something
(define (display-lazy-pair p)
  (define (display-promise obj)
    (if (promise? obj)
      (if (evaluated-promise? obj)
        ;; recursive call to handle nested pairs
        ;; (list is one kind of nested pair)
        (display-promise (promise-result obj))
        (display "<promise>"))
      ;; not a promise
      (display obj)))
  (display-promise (non-strict-car p))
  (display " ")
  (display-promise (non-strict-cdr p)))
```
