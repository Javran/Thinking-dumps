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

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(p->file (right-split einstein 4)
         "2_4_einstein_rsplit")

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let* ((up (up-split painter (- n 1)))
           (rt (right-split painter (- n 1)))
           (top-left (beside up up))
           (bottom-right (below rt rt))
           (corner (corner-split painter (- n 1))))
      (beside (below painter top-left)
              (below bottom-right corner)))))

(p->file (corner-split einstein 4)
         "2_4_einstein_csplit")

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(p->file (square-limit einstein 4)
         "2_4_einstein_slimit")
