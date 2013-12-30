(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

; an attempt of improving streams' readability
(define (gen-stream iterate s)
  ; gen-stream :: (s -> (a,s)) -> s -> [a]
  ; iterate    ::  s -> (a,s)
  (let ((result (iterate s)))
    (cons-stream
      (car result)
      (gen-stream iterate (cdr result)))))

(define odds
  (gen-stream
    (lambda (n)
      ; next value: 2n+1
      ; next state:  n+1
      (cons (+ n n 1)
            (+ n 1)))
    0))

(define fibs
  (gen-stream
    (lambda (pair)
      (let ((m (car pair))
            (n (cdr pair)))
        ; next value:   n
        ; next state: m+n
        (cons n
              (cons n (+ m n)))))
    '(0 . 1)))

(print-few 10 odds)
(print-few 10 fibs)

(end-script)
