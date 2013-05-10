(load "../common/utils.scm")

; (good-enough? guess) => #t / #f
; (improve guess) => improve a guess
(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
      guess
      (try (improve guess))))
  try)

(define (average-damping f)
  (lambda (x)
    (average x (f x))))

; sqrt: find y s.t. for a given x, y^2 = x => y = x/y
(define (my-sqrt x)
  (define (good-enough? y)
    (< (abs (- (square y) x))
       0.00001))
  (define (improve guess)
    ((average-damping (lambda (y) (/ x y))) guess))
  ((iterative-improve good-enough? improve) 1.0))

(out (my-sqrt (square 256)))
; ~= 256
(out (my-sqrt (square 1234)))
; ~= 1234

; if v-pair is a pair, it stores (old-value . cur-value)
; else v-pair is the init guess
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? v-pair)
    (if (pair? v-pair)
      (< (abs (- (car v-pair)
                 (cdr v-pair)))
         tolerance)
      ; else v-pair is the init guess, if we are lucky enough:
      (= (f v-pair) 0)))

  ; extract value from a v-pair
  (define (extract-value v-pair)
    (if (pair? v-pair)
      (cdr v-pair)
      v-pair))

  (define (improve v-pair)
    (if (pair? v-pair)
      (cons (cdr v-pair)
            (f (cdr v-pair)))
      (cons v-pair
            (f v-pair))))

  (extract-value
    ((iterative-improve good-enough? improve) 1.0)))

; reimpl my-sqrt-fp using fixed-point
(define (my-sqrt-fp x)
  (fixed-point (average-damping
                 (lambda (y) (/ x y)))
               1.0))

(out (my-sqrt-fp 1234))
; ~= 35.128

; TODO: further more ... reimpl ex 1.7

(end-script)
