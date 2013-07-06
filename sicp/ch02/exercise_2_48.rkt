#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

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

; ==== start solution ====

(define make-segment1 cons)
(define start-segment1 car)
(define end-segment1 cdr)

(let* ((v1 (make-vect1 1 2))
       (v2 (make-vect1 4 8))
       (seg (make-segment1 v1 v2)))
  (display "start: ")
  (print-vect1 (start-segment1 seg))
  (display "end: ")
  (print-vect1 (end-segment1 seg)))
