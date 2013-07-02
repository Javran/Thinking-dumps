(load "../common/utils.scm")

; (flatmap
;   (lambda (new-row)
;     (map (lambda (rest-of-queens)
;            (adjoin-position new-row k rest-of-queens))
;          (queen-cols (- k 1))))
;   (enumerate-interval 1 board-size))

; the interchange leads to unnecessary calculations:
; in this case, whenever a `new-row` is given,
;   we need to re-solve the problem of the placement of the first (k-th) queens again
; while we only need to solve it only once before enumerate `new-row`

; assume f1(k) is the time consumption for a normal `(queens n)`
; then approximately, f1(k) = f1(k-1) * n ~= n^k = T
; for each level of k, f2(k) need to solve f2(k-1) for n times => (n^2)^k ~= T^2

(end-script)
