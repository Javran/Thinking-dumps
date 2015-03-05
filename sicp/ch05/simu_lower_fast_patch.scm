;; we are missing details of the implementation
;; so we just pretend we have them implmented correctly
;; since "(const <data>)" works for any data,
;; we can use (const ()) to make an empty list
(define (lower-ops-builder-extra m)
  `((car ,car)
    (cdr ,cdr)
    (set-car! ,set-car!)
    (set-cdr! ,set-cdr!)
    (cons ,cons)
    (null? ,null?)
    (pair? ,pair?)))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   (ops-builder-union
    lower-ops-builder-extra
    default-ops-builder)))
