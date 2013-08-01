(define (drop num)
  (let ((type (type-tag num))
        (data (contents num)))
    (let ((proc (get 'project type)))
      (if proc
        ; `project` operation is available
        (let ((lowered (project num)))
          (cond ((=zero? num)
                  (drop lowered))
                ((equ? (raise lowered) num)
                  ; `project` operation accepted, go futher
                  (drop lowered))
                (else num)))
        ; lowest reached
        num))))

(define (test-drop)
  (let ((testcases (list
                     (cons (list (make-complex-ri 1 0))
                           'integer)
                     (cons (list (make-complex-ri 5 2))
                           'complex)
                     (cons (list (make-real 1.2345))
                           'rational)
                     (cons (list (make-rational 5 2))
                           'rational)
                     (cons (list (make-rational 10 2))
                           'integer)))
        (f (lambda (x) (type-tag (drop x)))))
    (do-test f testcases)))
