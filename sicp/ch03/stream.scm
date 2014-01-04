(define (stream-enumerate-interval low high)
  (if (> low high)
    nil
    (cons-stream low (stream-enumerate-interval
                       (+ low 1)
                       high))))

(define list-in-range-stream
  stream-enumerate-interval)

(define (display-stream s)
  (stream-for-each display-line s))

(define display-line
  ; I personally do not like
  ;   to have newline outputed
  ;   before any meaningful info outputed
  out)

(define (integers-starting-from n)
  (cons-stream
    n
    (integers-starting-from (+ n 1))))

(define (take n stream)
  (if (or (= n 0) (stream-null? stream))
    the-empty-stream
    (cons-stream
      (stream-car stream)
      (take (- n 1) (stream-cdr stream)))))

(define (drop n stream)
  (if (or (= n 0) (stream-null? stream))
    stream
    (drop (- n 1) (stream-cdr stream))))

; print few elements from the stream, as a test
(define (print-few n stream)
  (out (stream->list (take n stream))))

(define (zip-streams-with proc)
  (lambda args
    (apply stream-map (cons proc args))))

(define add-streams (zip-streams-with +))

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map
    ((curry2 *) factor)
    stream))

(define (stream-sum s)
  ; (stream-sum s) = s0, s0+s1, s0+s1+s2, ...
  ;   where s = s0, s1, s2, ...
  (define stream-sum-aux
    (cons-stream
      0
      (add-streams s stream-sum-aux)))
  (drop 1 stream-sum-aux))
(define parital-sums stream-sum)

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (head s1)
      (interleave s2 (tail s1)))))

(define (pairs s t)
  (cons-stream
    (list (head s) (head t))
    (interleave
      (stream-map (lambda (x)
                    (list (head s) x))
                  (tail t))
      (pairs (tail s) (tail t)))))

(define (take-while pred s)
  (if (stream-null? s)
    the-empty-stream
    (if (pred (head s))
      (cons-stream
        (head s)
        (take-while pred (tail s)))
      the-empty-stream)))

(define (take-until pred s)
  (take-while (compose not pred) s))

(define (drop-while pred s)
  (if (stream-null? s)
    the-empty-stream
    (if (pred (head s))
      (drop-while pred (tail s))
      s)))

(define (drop-until pred s)
  (drop-while (compose not pred) s))

; calculate integral, return a stream s_i,
;   where s_i = init-val + sum(integrand_j * dt)
;         j goes from the first index to i
(define (integral integrand init-val dt)
  (define int
    (cons-stream
      init-val
      (add-streams
        (scale-stream integrand dt)
        int)))
  int)

(load "./stream-test.scm")
(run-stream-test)
