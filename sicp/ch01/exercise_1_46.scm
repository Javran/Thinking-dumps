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

; redo ex 1.7, i.e. judge if a guess is good enough by comparing
;     the relative difference between current guess and previous one
(define (my-sqrt-2 x)
  ; v-pair: if it's a pair, stores previous guess and current guess
  ;       : else it's a value (initial guess)
  (define tolerance 0.00001)

  (define (good-enough? v-pair)
    (if (pair? v-pair)
      (let ((old-guess (car v-pair))
            (cur-guess (cdr v-pair)))
        (< (abs (/ (- cur-guess old-guess)
                   cur-guess))
           tolerance))
      (= x (square v-pair))))

  (define (next guess)
    ((average-damping
       (lambda (y) (/ x y))) guess))

  (define (improve v-pair)
    (if (pair? v-pair)
      (cons (cdr v-pair)
            (next (cdr v-pair)))
      (cons v-pair
            (next v-pair))))

  (define (extract-value v-pair)
    (if (pair? v-pair)
      (cdr v-pair)
      v-pair))

  (extract-value 
    ((iterative-improve good-enough? improve) 1.0)))

(newline)
(out (my-sqrt-fp (square 1.234e-6)))
(out (my-sqrt    (square 1.234e-6)))
; poor performance for small numbers

(out (my-sqrt-2  (square 1.234e-6)))
; this one should be the best

(end-script)
