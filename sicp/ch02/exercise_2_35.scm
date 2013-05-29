(load "../common/utils.scm")

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (accumulate op initial seq)
  (if (null? seq)
    ; the accumulated result goes to `initial`
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

(define (count-leaves-2 t)
  (accumulate +
              0
              (map
                ; for list, do `count-leaves-2` recursively
                ; for any other thing, simply count it as 1
                (lambda (x)
                  (if (list? x)
                    (count-leaves-2 x)
                    1))
                t)))

(out (count-leaves x))
(out (count-leaves-2 x))
; 4

(out (count-leaves (list x x)))
(out (count-leaves-2 (list x x)))
; 8

(end-script)
