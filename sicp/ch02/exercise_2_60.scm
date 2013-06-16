(load "../common/utils.scm")

; exactly what I've done in "./3_3_represent_sets.scm"
;   which should be the simplest implementation

; a set is a data structure that supports:
; union-set
; intersection-set
; element-of-set?
; adjoin-set

(define union-set append)
(define (intersection-set list-a list-b)
  (filter
    (lambda (x) (memq x list-b))
    list-a))

(define (element-of-set? e s) 
  (if (memq e s)
    #t
    #f))

(define adjoin-set cons)

(let ((set1 (list-in-range 1 5))
      (set2 (list-in-range 3 7)))
  (out (map (lambda (x) (element-of-set? x set1)) (list-in-range 1 10)))
  ; #t 5 times, #f 5 times
  (out (adjoin-set 2 set2))
  ; (2 .. 7)
  (out (adjoin-set 3 set2))
  ; (cons 3 (3..7))
  (out (intersection-set set1 set2))
  ; (3 4 5) in common
  (out (union-set set1 set2))
  ; has duplicate elements
  )

; comparison:
;   element-of-set?
;     both are theta(n)
;   adjoin-set
;     duplicate impl: theta(1), non-duplicate impl: theta(n)
;   intersection-set
;     both are theta(n^2)
;   union-set
;     duplicate impl: theta(n), non-duplicate impl: theta(n^2)

; adjoin-set & union-set is quicker for duplicate impl despite that we might have a big 'n'
; when we don't have much element, and when it doesn't matter if we have duplicate elements stored in memory
;   I'd prefer this representation
(end-script)
