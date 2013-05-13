(load "../common/utils.scm")

(define make-point  cons)
(define x-point     car)
(define y-point     cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; represent rectangle in two ways
; 1. rect-1, using a pair of points,
;     one for left-down corner and another for right-up corner
; 2. rect-2, using a point for the left-down corner and a pair of number: (width . height)

; in order to make the perimeter and area procedures work properly for all the representations,
; I'll keep the data structure like: (data . procedure pair)
;     where "data" stands for the inner data representation described above
;     "procedure pair" keeps two getters for width & height

; get the data part, i.e. inner data structure
(define rect-data car)

; get the getters from "instance"
(define rect-width-getter  cadr) ; (_ . (<here> . _) )
(define rect-height-getter cddr) ; (_ . (_ . <here>) )

; now we can come up with procedures that calculate perimeter and area
(define (rect-perimeter inst)
  (* (+ ((rect-width-getter  inst) inst)
        ((rect-height-getter inst) inst))
     2))

(define (rect-area inst)
  (* ((rect-width-getter  inst) inst)
     ((rect-height-getter inst) inst)))

; we can even show the detail of a rectangle:
(define (rect-detail inst)
  (let ((w ((rect-width-getter  inst) inst))
        (h ((rect-height-getter inst) inst))
        (data (rect-data inst)))

    (display "Rect represented by: ")
    (display data)
    (display ", ")
    (display "width=")
    (display w)
    (display ", ")
    (display "height=")
    (display h)
    (newline)))



