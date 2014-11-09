;; we are missing details of the implementation
;; so we just pretend we have them implmented correctly
;; since "(const <data>)" works for any data,
;; we can use (const ()) to make an empty list
(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `((car ,car)
        (cdr ,cdr)
        (set-car! ,set-car!)
        (set-cdr! ,set-cdr!)
        (cons ,cons)
        (null? ,null?)
        (pair? ,pair?)
        ,@(old-builder m)))))
