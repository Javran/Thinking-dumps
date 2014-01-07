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

; break a stream into groups, making each group
;   has `n` successive elements from the original stream
;   groups are also streams
(define (group-stream n s)
  (let ((splitted (split n s)))
    (cons-stream (car splitted)
                 (group-stream n (cdr splitted)))))

(stream-for-each
  (compose out stream->list)
  (take 4 (group-stream 5 random-numbers)))

(end-script)
