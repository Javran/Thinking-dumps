;; for "ignore-errors" "condition?"
;; see: https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_17.html
(define (apply-primitive-procedure proc args)
  (let ((result (ignore-errors
                 (lambda ()
                   (apply (primitive-implementation proc) args)))))
    (if (condition? result)
        (make-error 'primitive-error result)
        result)))
