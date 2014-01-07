(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_82_common.scm")

; >>>> from "./exercise_3_5.scm"

; return a random in range [low, high)
;   given that if the number passed to random is exact,
;   the result can only be integers
;   so we need to make sure the number passed should be decimal
;   instead of integer
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (exact->inexact range)))))

(define (in-circle? x y)
  ; test if (x,y) is inside the circle
  ; x^2 + y^2 <= 1^2
  ; => x^2 + y^2 <= 1
  (<= (+ (square x) (square y)) 1))

; <<<< from "./exercise_3_5.scm"

; >>>> from "./3_5_5_modularity_of_fp_and_modularity_of_objs.scm"

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      ; return a stream of frac
      (/ passed (+ passed failed))
      (monte-carlo
        (tail experiment-stream)
        passed failed)))
  (if (head experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

; >>>> from "./3_5_5_modularity_of_fp_and_modularity_of_objs.scm"

(define (estimate-integral pred x1 x2 y1 y2)
  ; from a value within [0,1) to a value within [low,high)
  (define (unit->number low high percent)
    ; v = percent * (high-low) + low
    ; v_min = low
    ; v_max = high
    (+ low (* percent
              (- high low))))
  ; return a stream of lists of x and y within range
  (define random-x-y-nums
    (stream-map
      (lambda (x-y-stream)
        (let ((x-y-list (stream->list x-y-stream)))
          (list (unit->number x1 x2 (car x-y-list))
                (unit->number y1 y2 (cadr x-y-list)))))
      (group-stream 2 random-units)))
  (define experiment-stream
    (stream-map
      ((curry2 apply) pred)
      random-x-y-nums))
  (stream-map
    (lambda (p)
      (* p
         (- x2 x1)
         (- y2 y1)))
    (monte-carlo experiment-stream 0 0)))

(define estimate-pi
  (estimate-integral in-circle? -2 2 -2 2))

(out (exact->inexact (stream-ref estimate-pi 10000)))

(end-script)
