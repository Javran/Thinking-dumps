#lang eopl

(require "./common.rkt")

; number->sequence: Int -> NodeInSeq Int
; usage: create a list zipper containing `x`
(define (number->sequence x)
  (list x '() '()))

; current-element: NodeInSeq Int -> Int
; usage: return the current focusing element
(define current-element car)

; move-to-left: NodeInSeq Int -> NodeInSeq Int
; usage: move the focus to the left
(define (move-to-left zipper)
  (let ((cur (car zipper))
        (left-rev (cadr zipper))
        (right (caddr zipper)))
    (assert
      (not (null? left-rev))
      "already focusing at the leftmost element")
    (list (car left-rev)
          (cdr left-rev)
          (cons cur right))))

; move-to-right: NodeInSeq Int -> NodeInSeq Int
; usage: move the focus to the right
(define (move-to-right zipper)
  (let ((cur (car zipper))
        (left-rev (cadr zipper))
        (right (caddr zipper)))
    (assert
      (not (null? right))
      "already focusing at the rightmost element")
    (list (car right)
          (cons cur left-rev)
          (cdr right))))

; insert-to-left: Int x NodeInSeq Int -> NodeInSeq Int
; usage: insert an element to the left of current focus
(define (insert-to-left x zipper)
  (let ((cur (car zipper))
        (left-rev (cadr zipper))
        (right (caddr zipper)))
    (list cur
          (cons x left-rev)
          right)))

; insert-to-right: Int x NodeInSeq Int -> NodeInSeq Int
; usage: insert an element to the right of current focus
(define (insert-to-right x zipper)
  (let ((cur (car zipper))
        (left-rev (cadr zipper))
        (right (caddr zipper)))
    (list cur
          left-rev
          (cons x right))))

(let ((the-zipper
        '(6 (5 4 3 2 1) (7 8 9))))
  (out (number->sequence 7)
       ; (7 () ())
       (current-element the-zipper)
       ; 6
       (move-to-left the-zipper)
       ; (5 (4 3 2 1) (6 7 8 9))
       (move-to-right the-zipper)
       ; (7 (6 5 4 3 2 1) (8 9))
       (insert-to-left 13 the-zipper)
       ; (6 (13 5 4 3 2 1) (7 8 9))
       (insert-to-right 13 the-zipper)
       ; (6 (5 4 3 2 1) (13 7 8 9))
       ))
