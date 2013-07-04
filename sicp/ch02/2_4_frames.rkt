#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

; each frame is described by 3 vectors,
;   one indicating its offest to the screen origin(or something equivalent)
;   and another two shows the two edges of the frame
; we can use frame to shift and scale images

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))
; here we need implementation of:
; * add-vect
; * origin-frame
; * edge{1,2}-frame
; * scale-vect
; * {x,y}cor-vect
