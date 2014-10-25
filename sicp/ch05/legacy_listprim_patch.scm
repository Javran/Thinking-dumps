(define default-primitive-list
  (let ((old-primitive-list default-primitive-list))
    (lambda ()
      `((car ,car)
        (cdr ,cdr)
        (cons ,cons)
        (null? ,null?)
        (pair? ,pair?)
        ,@(old-primitive-list)))))
