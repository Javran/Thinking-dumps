#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

(define (transform-painter1 painter origin corner1 corner2)
  (define sub-vect vector-sub)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz1 painter)
  (transform-painter1
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))

(p->file (flip-horiz1 einstein)
         "ex_2_50_einstein_fh")

