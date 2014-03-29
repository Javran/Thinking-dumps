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

(define (map-successive-pairs f s)
  (stream-map
    (compose
      ; 3. apply f
      ((curry2 apply) f)
      ; 2. convert back to lists
      stream->list)
    ; 1. group by 2
    (group-stream 2 s)))

(define (coprime? a b)
  (= (gcd a b) 1))

(define cesaro-stream
  (map-successive-pairs
    coprime?
    random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      ; return a stream of frac
      (/ passed (+ passed failed))
      (monte-carlo
        (tail experiment-stream)
        passed failed)))
  (if (head experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(define estimated-pi
  (stream-map
    (lambda (p) (sqrt (/ 6 p)))
    (stream-filter
      ; occasionally we have a "divde by zero" error here
      ;   I just drop these elements from stream
      (arith 'ne 0)
      (monte-carlo cesaro-stream 0 0))))

(out (stream-ref estimated-pi 10000))

(end-script)
