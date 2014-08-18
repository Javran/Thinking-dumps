;; modify the specify element in the vector
;; by calling a procedure
(define (vector-modify! vec k proc)
  (vector-set!
   vec
   k
   (proc (vector-ref vec k))))

;; remove duplicate elements
(define (remove-duplicates xs)
  (if (null? xs)
      '()
      (cons (car xs)
            (delete
             (car xs)
             (remove-duplicates (cdr xs))))))

;; check if two lists are of the same length
;; (should be more efficient than getting their lengths
;; and then comparing)
(define (same-length? xs ys)
  (cond ((null? xs) (null? ys))
        ;; when we say "pair?", we actually checks
        ;; if "car" and "cdr" can be used on a given object.
        ((and (pair? xs) (pair? ys))
         (same-length? (cdr xs) (cdr ys)))
        (else #f)))
