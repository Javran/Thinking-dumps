(load "../common/utils.scm")

; a set is a data structure that supports:
; union-set
; intersection-set
; element-of-set?
; adjoin-set

; I'd like to implement the simpliest one: represent by list

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

(let* ((ls (union-set '(1 2 3) '(3 4 6)))
       (ls2 (adjoin-set 5 ls)))
  (out ls)
  (out (map (lambda (x) (element-of-set? x ls))
            (list-in-range 1 10)))
  (out (map (lambda (x) (element-of-set? x ls2))
            (list-in-range 1 10)))
  )

(end-script)
