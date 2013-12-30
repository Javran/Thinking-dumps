(define (div-series s1 s2)
  ; s1 / s2 = s1 * (1/s2)
  ; let s2p * c = s2; c = (car s2)
  ; s1 / s2 = s1 * (1/s2p * 1/c)
  ; s1 / s2 = 1/c * (s1 * 1/s2p)
  (define c (stream-car s2))
  (assert (not (= c 0))
          "s2 should have non-zero constant term")
  (define s2p (scale-stream s2 (/ 1 c)))
  (scale-stream
    (mul-series
      s1
      (invert-unit-series s2p))
    (/ 1 c)))
