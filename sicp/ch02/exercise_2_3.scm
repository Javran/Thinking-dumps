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
; 1. rect-pp, using a pair of points,
;     one for left-down corner and another for right-up corner
; 2. rect-pwh, using a point for the left-down corner and a pair of number: (width . height)

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

(define (rect-pp-ld inst)
  (car (rect-data inst)))

(define (rect-pp-ru inst)
  (cdr (rect-data inst)))

(define (rect-pp-make p-ld p-ru)
  (define (rect-width inst)
    (- (x-point (rect-pp-ru inst))
       (x-point (rect-pp-ld inst))))
  (define (rect-height inst)
    (- (y-point (rect-pp-ru inst))
       (y-point (rect-pp-ld inst))))
  (cons (cons p-ld p-ru)
        (cons rect-width rect-height)))

(rect-detail (rect-pp-make (make-point 2 3) (make-point 10 20)))
; w = 8, h = 17

; data: (pt . (w . h))
(define (rect-pwh-make p w h)
  (define (rect-width inst)
    (cadr (rect-data inst)))
  (define (rect-height inst)
    (cddr (rect-data inst)))
  (cons (cons p (cons w h))
        (cons rect-width rect-height)))

(rect-detail (rect-pwh-make (make-point 1 1) 5 6))
(newline)

; tests:
; rectangle #1: (1.23,4.56) - (7.89, 4.57)
(define rect-1
  (rect-pp-make (make-point 1.23 4.56)
                (make-point 7.89 4.57)))

; rectangle #2: (0,1), w=1.35, h=2.46
(define rect-2
  (rect-pwh-make (make-point 0 1) 1.35 2.46))

(define (calc-perimeter-and-area inst)
  (rect-detail inst)
  (display "perimeter: ")
  (display (rect-perimeter inst))
  (newline)
  (display "area: ")
  (display (rect-area inst))
  (newline))

(for-each
  calc-perimeter-and-area
  (list 
    rect-1
    ; p=13.34 a=6.66e-2
    rect-2
    ; p=7.62 a=3.321
    ))
