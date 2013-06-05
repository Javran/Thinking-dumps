;;;;  HEND.SCM
;;;; This is the code for the square-limit language

;;;; Representing frames

(define (make-frame origin edge1 edge2)
  (list 'frame origin edge1 edge2))

(define frame-origin cadr)
(define frame-edge1 caddr)
(define frame-edge2 cadddr)

;;;; Primitive painters

;;;The following procedures create primitive painters.
;;;They are defined in the file primitive-painters, which is compiled
;;;so that things will run fast.  You need not deal with the
;;;implementation of these procedures, just use them as black boxes.

;;;construct a painter from a number
;;;(define (number->painter num) ....)

;;;construct a painter from a procedure
;;;(define (procedure->painter proc) ....)

;;;construct a painter from a list of segments
;;;(define (segments->painter segments) ....)

;;;construct a painter from a Scheme picture
;;;(define (picture->painter picture) ....)

;;;The following procedure loads a painter from a image file in the
;;;6001-image directory 

(define (load-painter file-name)
  (picture->painter
   (pgm-file->picture
    (string-append "~u6001/6001-images/" file-name ".pgm"))))

;;; Some simple painters

(define black (number->painter 0))

(define white (number->painter 255))

(define gray (number->painter 150))

(define diagonal-shading
  (procedure->painter (lambda (x y) (* 100 (+ x y)))))

(define mark-of-zorro
  (let ((v1 (make-vect .1 .9))
        (v2 (make-vect .8 .9))
        (v3 (make-vect .1 .2))
        (v4 (make-vect .9 .3)))
    (segments->painter
     (list (make-segment v1 v2)
           (make-segment v2 v3)
           (make-segment v3 v4)))))

(define fovnder (load-painter "fovnder"))

;;;; Painting images on the screen

(define (paint window painter)
  (if (not (graphics-device? window))
      (error "bad window" window))
  (set-painter-resolution! 128)
  (painter (screen-frame))
  (picture-display window *the-screen* 0 256))

(define (paint-hi-res window painter)
  (if (not (graphics-device? window))
      (error "bad window" window))
  (set-painter-resolution! 256)
  (painter (screen-frame))
  (picture-display window *the-screen* 0 256))

(define (frame-coord-map frame)
  (lambda (point-in-frame-coords)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor point-in-frame-coords)
			       (frame-edge1 frame))
		 (vector-scale (vector-ycor point-in-frame-coords)
			       (frame-edge2 frame))))))

(define (make-relative-frame origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(make-frame new-origin
		    (vector-sub (m corner1) new-origin)
		    (vector-sub (m corner2) new-origin))))))


(define (transform-painter origin corner1 corner2)
  (lambda (painter)
    (compose painter
	     (make-relative-frame
	      origin
	      corner1
	      corner2))))

;;;; Basic means of combination for painters

(define flip-horiz
  (transform-painter (make-vect 1 0)
		     (make-vect 0 0)
		     (make-vect 1 1)))

(define rotate90
  (transform-painter (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect .5 0)))
    (superpose
     ((transform-painter zero-vector
			 split-point
			 (make-vect 0 1))
      painter1)
     ((transform-painter split-point
			 (make-vect 1 0)
			 (make-vect .5 1))
      painter2))))


(define rotate180 (repeated rotate90 2))
(define rotate270 (repeated rotate90 3))



(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))

(define (superpose painter1 painter2)
  (lambda (frame)
    (painter1 frame)
    (painter2 frame)))

;;; More complex means of combination

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

