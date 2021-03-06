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
  (* (monte-carlo trials experiment)
     (- x2 x1)
     (- y2 y1)))

(define (in-circle? x y)
  ; test if (x,y) is inside the circle
  ; x^2 + y^2 <= 1^2
  ; => x^2 + y^2 <= 1
  (<= (+ (square x) (square y)) 1))

(define (estimate-pi trials)
  (exact->inexact (estimate-integral in-circle? -2 2 -2 2 trials)))

(out (estimate-pi 10000))

(end-script)
