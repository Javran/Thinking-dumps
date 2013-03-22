(load "../common/utils.scm")

(define-syntax my-or1
  (rsc-macro-transformer
    (let ((xfmr (lambda (x y)
                  `(if ,x ,x ,y))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(out (my-or1 1 2))
(out (my-or1 #f 2))

; my-or might evaluate the first argument twice

(my-or1 (out "this is true!") 2)
; output the string twice
(newline)

(define-syntax my-or2
  (rsc-macro-transformer
    (let ((xfmr (lambda (x y)
                  `(let ((temp ,x))
                     (if temp temp ,y)))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(my-or2 (out "this is true!") 2)
; well done, first argument get eval-ed only once
(newline)

; but:
(define temp 3)
(out (my-or2 #f temp))
; returns #f while it's supposed to be 3

(define-syntax my-or3
  (rsc-macro-transformer
    (let ((xfmr (lambda (x y)
                  (let ((temp (gensym)))
                    `(let ((,temp ,x))
                       (if ,temp ,temp ,y))))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(out (my-or3 #f temp))
