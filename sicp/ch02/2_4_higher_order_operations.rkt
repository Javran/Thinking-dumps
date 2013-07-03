#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four
                    identity
                    flip-vert
                    identity
                    flip-vert)))
    (combine4 painter)))

(p->file (flipped-pairs einstein)
         "2_4_einstein_fp")

(define (square-limit-1 painter)
  (let ((combine4 (square-of-four
                    flip-horiz
                    identity
                    rotate180
                    flip-vert)))
    (combine4 einstein)))

(p->file (square-limit-1 einstein)
         "2_4_einstein_sl1")
