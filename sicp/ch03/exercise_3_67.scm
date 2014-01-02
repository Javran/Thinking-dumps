(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

; (s0,t0) | (s0,t1)  (s0,t2)
; --------+-----------------
; (s1,t0) | (s1,t1)  (s1,t2)
; (s2,t0) | (s2,t1)  (s2,t2)

(define (pairs s t)
  (cons-stream
    (list (head s) (head t))
    (interleave
      (interleave
        (stream-map
          (lambda (x)
            (list (head s) x))
          (tail t))
        (stream-map
          (lambda (y)
            (list y (head t)))
          (tail s)))
      (pairs (tail s) (tail t)))))

(print-few 20 (pairs integers integers))

(end-script)
