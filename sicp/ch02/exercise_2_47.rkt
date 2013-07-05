#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

(define (test-procedures
          make-frame
          origin-frame
          edge1-frame
          edge2-frame)
  (define (assert t)
    (if (not t)
      (error "assertion failed\n")
      (display "assertion passed\n")))
  (let ((fr (make-frame 'o-frame 'e1-frame 'e2-frame)))
    (display "test origin frame ... \n")
    (assert (equal? (origin-frame fr) 'o-frame))
    (display "test edge 1 frame ... \n")
    (assert (equal? (edge1-frame fr) 'e1-frame))
    (display "test edge 2 frame ... \n")
    (assert (equal? (edge2-frame fr) 'e2-frame))
    (display "test done.\n")))

(define (make-frame-a origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame-a car)
(define edge1-frame-a cadr)
(define edge2-frame-a caddr)

(define (make-frame-b origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-b car)
(define edge1-frame-b cadr)
(define edge2-frame-b cddr)

(display "test #1:\n")
(test-procedures 
  make-frame-a
  origin-frame-a
  edge1-frame-a
  edge2-frame-a)

(display "test #2:\n")
(test-procedures
  make-frame-b
  origin-frame-b
  edge1-frame-b
  edge2-frame-b)
