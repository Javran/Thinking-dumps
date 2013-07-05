#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

; seems re-define procedures(e.g. make-vect)
;   is not allowed by default in racket,
;   we add "1" to each procedure instead.

(define make-vect1 cons)
(define xcor-vect1 car)
(define ycor-vect1 cdr)

(define (add-vect1 v1 v2)
  (make-vect1 (+ (xcor-vect1 v1)
                 (xcor-vect1 v2))
              (+ (ycor-vect1 v1)
                 (ycor-vect1 v2))))

(define (scale-vect1 s v)
  (make-vect1 (* s (xcor-vect1 v))
              (* s (ycor-vect1 v))))

(define (sub-vect1 v1 v2)
  ; a-b = a+ (-1)*b
  (add-vect1 v1
             (scale-vect1 -1 v2)))

(define (print-vect1 v)
  (for-each 
    display
    (list "(" (xcor-vect1 v) ", " (ycor-vect1 v) ")" #\newline)))

(let ((v1 (make-vect1 1 10))
      (v2 (make-vect1 4 40)))
  (print-vect1 v1)
  ; (1, 10)
  (print-vect1 v2)
  ; (4, 40)
  (print-vect1 (scale-vect1 5 v1))
  ; (5, 50)
  (print-vect1 (sub-vect1 (scale-vect1 3 v1) v1))
  ; (2, 20)
  )
