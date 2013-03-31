(load "../common/utils.scm")
(load "../common/amb.scm")

; actually amb returns any one from the alternatives
; but here 'amb' returns the first thing that can eval without failure

(out (amb 2 4))
; 2

(out (amb (amb (amb (amb) "nice" "boat"))))
; nice

(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
        (amb i (loop (+ i 1)))))))
; generates: (amb lo (amb (+ lo 1) (amb (+ (+ lo 1) 1) ... )))

(out (number-between 10 12))
; 10

; the magic is ...
(let loop ((i 0))
  (if #t
    (begin
      ; amb is special ... it breaks the loop quietly ...
      (out (amb))
      (loop (+ i 1)))))
; only 11 is printed

(out (amb))
; 12
(out (amb))
; boat
; ok, here we've noticed amb might not fail
;     unless EVERYTHING put into the amb seq got failed

; this definition of 'assert' looks stupid,
;     why not just simply call 'error' but do all the things indirectly?
;     moreover, making direct use of 'error' gives us the ability
;     of indicating the reason of a specific failure
(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(out (bag-of
  (number-between 1 10)))
