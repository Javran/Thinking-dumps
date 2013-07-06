#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

(define (draw-line a b)
  ; placeholder: draw-line from a to b
  #f)

(define start-segment #f)
(define end-segment #f)

; add a "-1" to get rid of confliction
(define (segments->painter-1 segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))

