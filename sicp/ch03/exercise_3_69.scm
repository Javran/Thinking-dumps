(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

; since `(pairs integers integers)` generates `(i,j)`
;   where `i <= j`
; we fix the first element, and use `pair` to generate the rest part:
; e.g., the first element = 1:
;   (1,(1,1)), (1,(1,2)), (1,(1,3)), ...
;              (1,(2,2)), (1,(2,3)), ...
;                         (1,(3,3)), ...

(define (triples s t u)
  (define pair-stream (pairs t u))
  (cons-stream
    (cons (head s) (head pair-stream))
    (interleave
      (stream-map
        (lambda (rest)
          (cons (head s) rest))
        (tail pair-stream))
      (triples (tail s) (tail t) (tail u)))))

(print-few 20 (triples integers integers integers))

(define pythagrean-triples
  (stream-filter
    (lambda (triple)
      (let ((a (car triple))
            (b (cadr triple))
            (c (caddr triple)))
        (= (+ (* a a) (* b b))
           (* c c))))
    (triples integers integers integers)))

(print-few 4 pythagrean-triples)

(end-script)
