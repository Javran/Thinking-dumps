(load "../common/utils.scm")

(define (accumulate op initial seq)
  (if (null? seq)
    ; the accumulated result goes to `initial`
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))


(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coeff-seq))

(out (horner-eval 2 (list 1 3 0 5 0 1)))
; 79

(out (horner-eval 2.3 (list 1 2 3 4)))
; ~= 70.138

(end-script)
