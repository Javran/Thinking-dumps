#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

; levels of language for rubust design
; * stratified design
; * a complex system should be structured as a sequence of levels
;     that are described using a sequence of languages.
; * each level uses stuffs from previous level as primitives,
;     and each level provides primitives for the next level.

; * rubust: small changes in a specification will require correspondingly
;     small changes in the program
