(load "../common/utils.scm")

(define (gen-list from to step)
  (if (> from to)
    '()
    (cons from (gen-list (+ from step) to step))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter f acc counter)
    (cond ((= counter 0)
           acc)
          ((odd? counter)
           (repeated-iter f (compose f acc) (- counter 1)))
          (else ; even
            (repeated-iter (compose f f) acc (/ counter 2)))))
  (repeated-iter f identity n))

; use large dx to show the difference
(define dx 0.001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

(define (f x)
  (sin (exp x)))

(define fs
  (n-fold-smoothed f 7))

(call-with-output-file "exercise_1_44_original.data"
  (lambda (p)
    (for-each
      (lambda (x)
        (display x p)
        (display " " p)
        (display (f x) p)
        (newline p))
      (gen-list 5 6 0.001))))

(call-with-output-file "exercise_1_44_smooth_7.data"
  (lambda (p)
    (for-each
      (lambda (x)
        (display x p)
        (display " " p)
        (display (fs x) p)
        (newline p))
      (gen-list 5 6 0.001))))

; the result has been generated as "exercise_1_44_result.png"
; you need gnuplot to get the chart:
; first run this program to generate "exercise_1_44_*.data"
; then run gnuplot script to generate "exercise_1_44_result.png"
