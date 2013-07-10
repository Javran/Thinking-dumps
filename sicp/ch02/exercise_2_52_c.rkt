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

; the original version, when applied with "einstein"
;   the result looks like looking outward
(define (square-limit1 painter)
  (let ((combine4 (square-of-four
                    flip-horiz
                    identity
                    rotate180
                    flip-vert)))
    (combine4 painter)))

; here comes square-limit2 that would make the result looking inward
(define (square-limit2 painter)
  (let ((combine4 (square-of-four
                    identity
                    flip-horiz
                    flip-vert
                    rotate180)))
    (combine4 painter)))

(p->file (square-limit1 einstein) "ex_2_52_c_sl1")
(p->file (square-limit2 einstein) "ex_2_52_c_sl2")
