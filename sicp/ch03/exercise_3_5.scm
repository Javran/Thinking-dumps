(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

; return a random in range [low, high)
;   given that if the number passed to random is exact,
;   the result can only be integers
;   so we need to make sure the number passed should be decimal
;   instead of integer
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (exact->inexact range)))))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (pred x y)))
  (monte-carlo trials experiment))

(end-script)
