(load "../common/utils.scm")

; TODO: can we simplify the macro definition?
(define-syntax when
  (rsc-macro-transformer
    (let ((xfmr (lambda (test . branch)
		  (list 'if test
                    (cons 'begin branch)))))
      (lambda (e r)
	(apply xfmr (cdr e))))))

(when #t
  (out "this will be displayed when the condition is met"))
