(load "../common/utils.scm")

; compose-free version
(define (repeated-1 f time)
  (if (= time 0) 
    identity
    (lambda (x)
      (f ((repeated-1 f (- time 1)) x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-2 f time)
  (if (= time 0)
    identity
    (compose f (repeated-2 f (- time 1)))))

(out ((repeated-1 square 2) 5))
(out ((repeated-2 square 2) 5))

; use a procedure that has side-effect
(define (p-with-side-effect x)
  (out "a procedure that has side effect"))

((repeated-1 p-with-side-effect 4) 'stub)
; 4 times
(newline)

((repeated-2 p-with-side-effect 4) 'stub)
; 4 times
