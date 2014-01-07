(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

; >>>> from 3.1.2
(define random-range 1000000)
(define rand-result car)
(define rand-state cdr)
(define mk-rand-st cons)

(define (rand-update pair)
  (let ((st (rand-state pair)))
    ; since random-state object get updated when called by random
    ;   simply pass the old one should be ok
    (mk-rand-st (random random-range st)
                st)))

(define random-init (mk-rand-st nil (make-random-state)))
; <<<< from 3.1.2

(define random-numbers
  ; since rand-update returns a pair (a,s),
  ;   we only need the result, so let's hide state inside.
  (let ()
    (define random-number-pair-stream
      (cons-stream
        random-init
        (stream-map rand-update random-number-pair-stream)))
    ; the first one is null, so we skip it.
    (tail
      (stream-map
        rand-result
        random-number-pair-stream))))

(end-script)
