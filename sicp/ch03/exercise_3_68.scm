(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (pairs s t)
  (interleave
    (stream-map
      (lambda (x)
        (list (stream-car s) x))
      t)
    (pairs (stream-cdr s) (stream-cdr t))))

; does not work because that
;   `interleave` is strict on its second argument.
;   will work if we implement `interleave` using syntactic sugar
;   like `cons-stream`
(print-few 10 (pairs integers integers))

(end-script)
