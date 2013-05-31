(load "../common/utils.scm")

(define (accumulate op initial seq)
  (if (null? seq)
    ; the accumulated result goes to `initial`
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

(define my-fold-right accumulate)

(let ((ls (list-in-range 1 100)))
  (out (my-fold-right + 0 ls)
       (fold-right + 0 ls)))
; 5050
(newline)

(define (my-fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter init seq))

(let ((ls (list-in-range 1 100)))
  (out (my-fold-left + 0 ls)
       (fold-left + 0 ls)))
; 5050

; (error "uncomment this line for answers")

(out (fold-right / 1 (list 1 2 3))
     ; => (1/ ( 2/ (3/1) ) => 3/2
     (fold-left / 1 (list 1 2 3))
     ; => 1/1 /2 /3 => 1/6
     (fold-right list nil (list 1 2 3))
     ; (1 (2 (3 nil)))
     (fold-left list nil (list 1 2 3))
     ; (((nil 1) 2) 3)
     )

; attempt to make a recursive definition of fold-left:
(define (my-fold-left-1 op init seq)
  (if (null? seq)
    init
    (my-fold-left-1 op (op init (car seq)) (cdr seq))))

(let ((ls (list-in-range 1 5)))
  (out (fold-left / 1 ls)
       (my-fold-left / 1 ls)
       (my-fold-left-1 / 1 ls)))
; 1/120

; when fold-left and fold-right produce the same values for any seq:
; if seq is null: already equal
; if seq is not null:
; (fold-left op (op init (car seq)) (cdr seq)) and
; (op (car seq) (fold-right op init (cdr seq))) should be equal
; => `(car seq)` to `hd`, `(cdr seq)` to `tl`
; (fold-left op (op init hd) tl) and
; (op hd (fold-right op init tl)) should be equal
; => change argument order
; (foldl' op tl (op init hd)) and
; (op hd (foldr' op tl init)) should be equal
; => `(foldl' op tl)` to `fl`, `(foldr' op tl)` to `fr`
; (fl (op init hd)) and
; (op hd (fr init)) should be equal
;
; I can't figure out anything further ... now let's guess:
; the difference of fold-left and fold-right are how heads are consumed
;     for fold-left, head is applied as the second argument
;     whereas for fold-right, head is applied as the first
;     so at least (op a (op b c)) should equal to (op (op a b) c)
;     and (op a b) should equal to (op b a) (I'm not so sure for this one)

(end-script)
