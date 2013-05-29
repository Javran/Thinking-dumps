(load "../common/utils.scm")

(define (accumulate op initial seq)
  (if (null? seq)
    ; the accumulated result goes to `initial`
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ; if the first list is empty, so will be the others
    ;     in this situation, we return nil
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(let ((s (list (list 1 2 3)
               (list 4 5 6)
               (list 7 8 9)
               (list 10 11 12))))
  (out (accumulate-n + 0 s)))
; (22 26 30)

(end-script)
