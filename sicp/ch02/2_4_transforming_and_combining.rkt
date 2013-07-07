#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

; sub-vect -> vector-sub according to:
; http://planet.plt-scheme.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
(define sub-vect vector-sub)

(define (transform-painter1 painter origin corner1 corner2)
  ; painter: the original painter
  ; origin, corner1, corner2: how to transform a frame
  (lambda (frame)
    ; make a transformer "m"
    (let ((m (frame-coord-map frame)))
      ; all coordinates are required to be transformed
      ; origin -> (m origin)
      ; corner1 -> (m corner1)
      ; corner2 -> (m corner2)
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

(define (flip-vert1 painter)
  (transform-painter1 painter
                      (make-vect 0 1) ; new origin
                      (make-vect 1 1) ; new edge1
                      (make-vect 0 0) ; new edge2
                      ))

(p->file (flip-vert1 einstein)
         "2_4_einstein_flip1")

(define (shrink-to-upper-right painter)
  (transform-painter1
    painter
    (make-vect 0.5 0.5)
    (make-vect 1 0.5)
    (make-vect 0.5 1)))

(p->file (shrink-to-upper-right einstein)
         "2_4_einstein_shrink")

(define (rotate90-1 painter)
  (transform-painter1
    painter
    (make-vect 1 0)
    (make-vect 1 1)
    (make-vect 0 0)))

(p->file (rotate90-1 einstein)
         "2_4_einstein_r90")

(define (squash-inwards painter)
  (transform-painter1
    painter
    (make-vect 0 0)
    (make-vect 0.3 0.2)
    (make-vect 0.2 0.3)))

(p->file (squash-inwards einstein)
         "2_4_einstein_si")

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

(p->file (beside1 einstein einstein)
         "2_4_einstein_beside1")
