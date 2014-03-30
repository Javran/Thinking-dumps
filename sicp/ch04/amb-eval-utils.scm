
;; test if the object is a non-empty list
;; and tagged is expected
(define (list-tagged-with? tag)
  (lambda (xs)
    (and (list? xs)
         (non-empty? xs)
         (eq? (car xs) tag))))
