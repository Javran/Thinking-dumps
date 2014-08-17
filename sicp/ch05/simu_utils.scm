;; modify the specify element in the vector
;; by calling a procedure
(define (vector-modify! vec k proc)
  (vector-set!
   vec
   k
   (proc (vector-ref vec k))))
