(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (my-append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

;; require "x" to be an non-empty list
(define (my-last-pair x)
  (if (null? (cdr x))
      x
      (my-last-pair (cdr x))))

;; require "x" to be an non-empty list
(define (my-append! x y)
  (set-cdr! (my-last-pair x) y)
  x)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:

