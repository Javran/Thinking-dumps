(load "../common/utils.scm")

(define-syntax-rule (my-when test branch)
  (if test
    (begin branch)))

(my-when #t
  (display "This string should be printed"))

(my-when #f
  (display "This string should not be printed"))

(newline)

(define-syntax-rule (my-unless test branch)
  (my-when (not test) branch))

(my-unless #t
  (display "This should not be printed"))

(my-unless #f
  (display "This should be printed"))

(newline)

(define-syntax when
  (rsc-macro-transformer
    (let ((xfmr (lambda (test . branch)
                  (list 'if test
                    (cons 'begin branch)))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(when #t
  (out "This appears to be true"))

(define-syntax unless1
  (rsc-macro-transformer
    (let ((xfmr (lambda (test . branch)
                  (list 'if
                    (list 'not test)
                    (cons 'begin branch)))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(unless1 #t
  (out "unless1-#t"))

(unless1 #f
  (out "unless1-#f"))

(define-syntax unless2
  (rsc-macro-transformer
    (let ((xfmr (lambda (test . branch)
                  (cons 'when
                    (cons (list 'not test) branch)))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(unless2 #t
  (out "unless2-#t"))

(unless2 #f
  (out "unless2-#f"))
