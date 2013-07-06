#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define (p->file painter filename)
  (send (send (paint painter) get-bitmap)
        save-file
        (string-append "./pic_out/" filename ".png")
        'png))

; list should be placed in format: '(((0.1 0.2) (0.3 0.4)) ... )
(define (list->segments point-pairs)
  (map
    (lambda (point-pair)
      (let* ((a (car point-pair))
             (b (cadr point-pair))
             (ax (car a))
             (ay (cadr a))
             (bx (car b))
             (by (cadr b)))
        (make-segment (make-vect ax ay)
                      (make-vect bx by))))
    point-pairs))

; place pattern "square-limit" here to show the final result more clearly
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))
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
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (test-painter p out-name)
  (p->file p out-name)
  (p->file (square-limit p 4) (string-append out-name "_sl")))

; coord:
; y
; ^
; |
; D   C
; |
; A---B--->x
; prefix: p

; using "1" as upbound will draw lines outside the output picture
(define pa '(0 0))
(define pb '(1 0))
(define pc '(1 1))
(define pd '(0 1))

(define painter_a
  (segments->painter
    (list->segments
      (list (list pa pb)
            (list pb pc)
            (list pc pd)
            (list pd pa)))))

(for-each
  (lambda (args)
    (apply test-painter args))
  (list
    (list painter_a "ex_2_49_a")))
