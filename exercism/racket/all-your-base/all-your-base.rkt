#lang racket

(require racket/control)

(provide rebase)

(define (rebase list-digits in-base out-base)
  (and
   (> in-base 1)
   (> out-base 1)
   (reset
    (let ([val (foldl
                (lambda (n acc)
                  (if (<= 0 n (- in-base 1))
                      (+  (* acc in-base) n)
                      ;; delimited continuation to fail-fast
                      (shift _ #f)))
                0
                list-digits)])
      (if (zero? val)
          '(0)
          (let loop ([digits '()]
                     [cur val])
            (if (zero? cur)
                digits
                (let-values ([(q r) (quotient/remainder cur out-base)])
                  (loop (cons r digits) q)))))))))