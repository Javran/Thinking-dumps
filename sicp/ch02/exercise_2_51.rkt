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

(define (beside1 painter1 painter2)
  (let* ((split-point (make-vect 0.5 0))
         (paint-left
           (transform-painter1
             painter1
             (make-vect 0 0)
             split-point
             (make-vect 0 1)))
         (paint-right
           (transform-painter1
             painter2
             split-point
             (make-vect 1 0)
             (make-vect 0.5 1))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

; define below in 2 different ways
; below1: analogous to the beside

(define (below1 painter1 painter2)
  (let* ((split-point (make-vect 0 0.5))
         (paint-down
           (transform-painter1
             painter1
             (make-vect 0 0)
             (make-vect 1 0)
             split-point))
         (paint-up
           (transform-painter1
             painter2
             split-point
             (make-vect 1 0.5)
             (make-vect 0 1))))
    (lambda (frame)
      (paint-down frame)
      (paint-up frame))))

; the upper would have two painters of Einstein
(p->file (below1 einstein (beside1 einstein einstein))
         "ex_2_51_einstein_below1")

; below2: in terms of beside and suitable rotation operations
(define (rotate-cc-270 painter)
  (transform-painter1
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)))

(define (rotate-cc-90 painter)
  (transform-painter1
    painter
    (make-vect 1 0)
    (make-vect 1 1)
    (make-vect 0 0)))

(define (below2 painter1 painter2)
  (let* ((rp1 (rotate-cc-90 painter1)) ; rotated painter1
         (rp2 (rotate-cc-90 painter2)) ; rotated painter2
         (beside-result (beside1 rp2 rp1))) ; note the order of p1-p2 here
                                            ;   which is due to the different drawing order of 
                                            ;   beside (left-right) and below (down-up)
    (rotate-cc-270 beside-result)))

(p->file (below2 einstein (beside1 einstein einstein))
         "ex_2_51_einstein_below2")
