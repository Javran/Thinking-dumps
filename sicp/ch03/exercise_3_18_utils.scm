(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; make a cycle `y` with proceeding line `x`
(define (make-lcycle x y)
  (set-cdr! (last-pair x) (make-cycle y))
  x)

(define x1 (make-cycle '(1 2 3 4)))
(define x2 (make-lcycle '(1 2 3) '(4 5 6)))
(define x3 '(1 2 3 4 5))
(define x4 (make-lcycle '(1 2 3 4 5 6) '(4 5 6 2 3 4 2 3 5 6)))

(define (contains-cycle-test f)
  (let ((testcases
          (list (mat x1 #t)
                (mat x2 #t)
                (mat x3 #f)
                (mat x4 #t))))
    (do-test contains-cycle? testcases))
  (out "Test done."))
