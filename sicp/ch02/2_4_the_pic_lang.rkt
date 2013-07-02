#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4 (below einstein2 einstein2))

(p->file einstein2 "2_4_einstein2")
(p->file einstein4 "2_4_einstein4")

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(p->file (flipped-pairs einstein4)
         "2_4_einstein16")
