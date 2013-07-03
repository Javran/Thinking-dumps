#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

; excerpted from "./2_4_the_pic_lang.rkt"

(define (right-split-1 painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split-1 painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split-1 painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split-1 painter (- n 1))))
      (below painter (beside smaller smaller)))))


; now we can easily come up with `split`:
(define (split op-2 op-1)
  (define (op painter n)
    (if (= n 0)
      painter
      (let ((smaller (op painter (- n 1))))
        (op-2 painter (op-1 smaller smaller)))))
  op)

(define right-split (split beside below))
(define up-split (split below beside))

(p->file (right-split einstein 3)
         "ex_2_45_einstein_rsplit")
(p->file (up-split einstein 3)
         "ex_2_45_einstein_usplit")

; extra task: left-split and down-split?
