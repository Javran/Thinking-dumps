(load "../common/utils.scm")

; to achieve a + (abs b), we should consider the sign of b,
; when b > 0, the expression will reduce to (+ a b)
; else b <= 0, the expression will reduce to (+ a (- b)), which is equal to (- a b)
; so we have:
; (if (> b 0)
;   (+ a b)
;   (- a b))
; so we can write this function as:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(out
  (a-plus-abs-b 10 (- 4))
  ; 14
  (a-plus-abs-b 10 4)
  ; 14
  (a-plus-abs-b (- 10) (- 10))
  ; 0
  )
