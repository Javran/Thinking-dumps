(load "../common/utils.scm")
(load "../common/test-utils.scm")

(let ()
  (define x '((a b) c d))
  (define y '(e f))
  (set-car! x y)
  (out x)
  ; ((e f) c d)
  (out y)
  ; (e f)
  )

(let ()
  (define x '((a b) c d))
  (define y '(e f))
  (set-cdr! x y)
  (out x)
  ; '((a b) e f)
  (out y)
  ; (e f)
  )

; I don't know how to implement `(get-new-pair)` correctly,
; So I decide to skip this part.

(end-script)
