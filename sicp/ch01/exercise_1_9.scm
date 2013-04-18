(load "../common/utils.scm")

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (add-1 a b)
  (if (= a 0)
    b
    (inc (add-1 (dec a) b))))

; => (add-1 4 5)
; => (if (= 4 0) _ (inc (add-1 (dec 4) 5)))
; => (inc (add-1 3 5))
; => (inc (if (= 3 0) _ (inc (add-1 (dec 3) 5))))
; => (inc (inc (if (= 2 0) _ (inc (add-1 (dec 2) 5)))))
; => (inc (inc (inc (if (= 1 0) _ (inc (add-1 (dec 1) 5))))))
; => (inc (inc (inc (inc (if (= 0 0) 5 _)))))
; => (inc (inc (inc (inc 5))))
; => (inc (inc (inc 6)))
; => (inc (inc 7))
; => (inc 8)
; => 9
; a linear recursive process

(define (add-2 a b)
  (if (= a 0)
    b
    (add-2 (dec a) (inc b))))

; => (add-2 4 5)
; => (if (= 4 0) _ (add-2 (dec 4) (inc 5)))
; => (add-2 3 6)
; => (if (= 3 0) _ (add-2 (dec 3) (inc 6)))
; => (add-2 2 7)
; => (if (= 2 0) _ (add-2 (dec 2) (inc 7)))
; => (add-2 1 8)
; => (if (= 1 0) _ (add-2 (dec 1) (inc 8)))
; => (add-2 0 9)
; => (if (= 0 0) 9 _)
; => 9
; a linear iteractive process

(out (add-1 4 5))
(out (add-2 4 5))
