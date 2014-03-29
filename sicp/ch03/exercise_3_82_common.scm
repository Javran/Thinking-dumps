; this part are modified
;   from "./3_5_5_modularity_of_fp_and_modularity_of_objs.scm"
;; modification #1: don't use random range, instread
;;   we just return a value from [0,1)
(define rand-result car)
(define rand-state cdr)
(define mk-rand-st cons)

(define (rand-update pair)
  (let ((st (rand-state pair)))
    ; since random-state object get updated when called by random
    ;   simply pass the old one should be ok
    ;; modification #2: see:
    ;; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Random-Numbers.html
    ;; we use `flo:random-unit` here
    (mk-rand-st (flo:random-unit st)
                st)))

(define random-init (mk-rand-st nil (make-random-state)))

(define random-units
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
