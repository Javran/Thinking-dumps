(load "../common/utils.scm")

(define-syntax when
  (rsc-macro-transformer
    (let ((xfmr (lambda (test . branch)
                  `(if ,test
                     (begin ,@branch)))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(when #t
  (out "when-#t"))

(when #f
  (out "when-#f"))
