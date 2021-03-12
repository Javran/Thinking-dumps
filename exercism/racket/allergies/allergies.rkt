#lang racket

(provide list-allergies allergic-to?)

(define allergic-table
  (list->vector
   (string-split "eggs peanuts shellfish strawberries tomatoes chocolate pollen cats")))

(define (list-allergies score)
  (let loop ([mask 1]
             [which 0]
             [xs '()])
    (if (>= which (vector-length allergic-table))
        xs
        (loop (+ mask mask)
              (add1 which)
              (if (zero? (bitwise-and mask score))
                  xs
                  (cons (vector-ref allergic-table which)
                        xs))))))
        
(define (allergic-to? str score)
  (and
   (member str (list-allergies score))
   #t))
