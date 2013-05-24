(load "../common/utils.scm")

(out (cons (list 1 2) (list 3 4)))
; given that (cons a b) inserts a in front of b
; we have:
(out (list (list 1 2) 3 4))

(out (equal? (cons (list 1 2) (list 3 4))
             (list (list 1 2) 3 4)))
; I think it is easiler to understand

(define x (cons (list 1 2) (list 3 4)))

(out (length x))
; 3

(define (my-count-leaves x)
  (cond ((non-empty? x)
          ; for non empty list, use `my-count-leaves` recursively
          ;   and add the results up
          (apply + (map my-count-leaves x)))
        ((list? x)
          ; an empty list, we simply return 0
          0)
        (else
          ; for other element, we have no knowledge about it
          ;   so we regard it as a single object   
          1)))
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(out (my-count-leaves x))
; 4
(out (count-leaves x))
; 4

(out (length (list x x)))
; 2

(out (my-count-leaves (list x x)))
; 8
(out (count-leaves (list x x)))
; 8

(end-script)
