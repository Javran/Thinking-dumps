(import (rnrs))

(use-modules ((ice-9 match)))

(define (min-max a b)
  (if (<= a b)
      (cons a b)
      (cons b a)))

(define (triangle a b c)
  ;; apply a 3-element sorting network,
  ;; that is, for input a, b, c
  ;; apply min-max to pair a-b, b-c, a-b, in that order
  ;; and we'll end up having (<= a b c).
  (match
   (min-max a b)
   [(a . b)
    (match
     (min-max b c)
     [(b . c)
      (match
       (min-max a b)
       [(a . b)
        (cond
         [(<= (+ a b) c) (raise 'invalid)]
         [(= a c) 'equilateral]
         [(or (= a b) (= b c)) 'isosceles]
         [else 'scalene])])])]))
