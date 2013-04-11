(define hex->char
  (lambda (x y)
    (integer->char
      (string->number (string x y) 16))))

(define url-decode
  (lambda (str)
    (let ((s (string->list str)))
      (list->string
        (let loop ((s s))
          (if (null? s)
            '()
            ; else
            (let ((head (car s))
                  (tail (cdr s)))
              (case head
                ((#\+)
                 ; '+' -> ' '
                 (cons #\space (loop tail)))
                ((#\%)
                 ; "%??" -> char
                 (cons (hex->char (car tail) (cadr tail))
                             (loop (cddr tail))))
                (else (cons head (loop tail)))))))))))

