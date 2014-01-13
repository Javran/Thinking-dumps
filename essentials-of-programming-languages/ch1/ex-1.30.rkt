#lang eopl

(require "../common.rkt")

; partition :: (a -> Bool, [Int]) -> ([Int], [Int])
; usage: do partition in `loi`, return a pair `(a,b)`
;   such that `pred` holds for any element from `a`
;   but `pred` fails for any element from `b`
(define (partition pred loi)
  (if (null? loi)
    (cons '() '())
    (let ((rest-result (partition pred (cdr loi))))
      (if (pred (car loi))
        (cons (cons (car loi)
                    (car rest-result))
              (cdr rest-result))
        (cons (car rest-result)
              (cons (car loi)
                    (cdr rest-result)))))))

; sort/predicate :: ((Int,Int) -> Bool, [Int]) -> [Int]
; usage: sort `loi` according to the predicate,
;   `(pred a b)` holds if and only if `a < b`
(define (sort/predicate pred loi)
  (if (null? loi)
    '()
    (let* ((pivot (car loi))
           (parts (partition (lambda (x) (pred x pivot)) (cdr loi)))
           (left-part (car parts))
           (right-part (cdr parts)))
      (append (sort/predicate pred left-part)
              (list pivot)
              (sort/predicate pred right-part)))))

(out 
     (sort/predicate < '(8 2 5 2 3))
     (sort/predicate > '(8 2 5 2 3))
     (sort/predicate > '(0 1 9 2 8 3 7 4 6 5))
     )
