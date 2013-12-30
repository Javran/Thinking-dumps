(define (constant-series c)
  (define zeros
    (cons-stream 0 zeros))
  (cons-stream c zeros))

(define (neg-stream s)
  (scale-stream s -1))

;        S * X = 1
; (1 + Sr) * X = 1
;   X + Sr * X = 1
;            X = 1 - Sr * X
(define (invert-unit-series s)
  (assert (= (stream-car s) 1)
          "constant term should be 1")
  ; Sr is of form:
  ;   Sr  = a_1 x^1 + a_2 x^2 + ...
  ; so the correct way of writing Sr is:
  ;   Sr  = (cons-stream 0 (stream-cdr s))
  ; which does not have a constant term
  ; so we make Sr' that Sr' * x = Sr, therefore:
  ;   Sr' = a_1 x^0 + a_2 x^1 + ...
  ; and Sr' can be written as:
  ;   Sr' = (stream-cdr s)
  ; thus:
  ;   X = 1 - Sr * X = 1 - (Sr' * X) * x
  ; the multiplication of `Sr'` and `X` can be done
  ;   using `mul-series`, and we multiple the result with `x`
  ;   to get the correct answer:
  ;   (Sr' * X) * x = (cons-stream 0 (mul-series sr' x))
  ; and now with the constant term
  ;   1             = (constant-series 1) = (1 0 0 0 ...)
  ; we can construct the correct answer
  (define inv-aux
    ; 1*x^0 - (Sr' * X)*x^1
    (cons-stream
      1
      (neg-stream
        (mul-series
          (stream-cdr s)
          inv-aux))))
  inv-aux)
