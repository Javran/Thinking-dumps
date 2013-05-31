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



(end-script)
