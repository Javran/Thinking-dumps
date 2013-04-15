(load "../common/utils.scm")

;(error "uncomment this line when the exercise is done")

(out
  10
  ; 10
  (+ 5 3 4)
  ; 12
  (- 9 1)
  ; 8
  (/ 6 2)
  ; 3
  (+ (* 2 4) (- 4 6))
  ; (+ 8 _2) -> 6
  )


(define a 3)
(define b (+ a 1))
; a -> 3
; b -> 4

(out
  (+ a b (* a b))
  ; (+ 3 4 12) -> 19
  (= a b)
  ; #f
  (if (and (> b a) (< b (* a b)))
  ; (b > a) and (b < (a*b)) ? -> a < b < a*b ? -> #t
    b
    a)
  ; 4
  (cond ((= a 4) 6) ; #f
        ((= b 4) (+ 6 7 a)) ; (+ 6 7 3) -> 16
        (else 25))
  ; 16
  (+ 2 (if (> b a) b a))
  ; (+ 2 4) -> 6
  (* (cond 
       ((> a b) a) ; #f
       ((< a b) b) ; #t
       (else -1))
     (+ a 1))
  ; (* 4 (+ 3 1)) -> 16
  )
