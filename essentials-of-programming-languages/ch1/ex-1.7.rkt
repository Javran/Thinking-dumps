#lang eopl

(require "../common.rkt")

(define nth-element-aux
  (lambda (l n)
    (if (null? l)
      #f
      (if (zero? n)
        (list (car l))
        (nth-element-aux (cdr l) (- n 1))))))

(define nth-element
  (lambda (l n)
    (let ((result (nth-element-aux l n)))
      (if result
        (car result)
        (insufficient-elements l (+ n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error
      'nth-element
      "List too short by ~s elements.~%"
      (+ n 1))))

(define insufficient-elements 
  (lambda (l n)
    (eopl:error
      'nth-element
      "~A does not have ~A elements"
      l n)))

(out (nth-element '(a b c) 7))
