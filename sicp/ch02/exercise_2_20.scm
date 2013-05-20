(load "../common/utils.scm")

(define (my-filter pred ls)
  (if (null? ls)
    '()
    (let ((hd (car ls))
          (tl (cdr ls)))
      (if (pred hd)
        (cons hd (my-filter pred tl))
        (my-filter pred tl)))))

(define (same-parity . ls)
  (let ((hd (car ls))
        (tl (cdr ls)))
    (cons hd
          (if (odd? hd)
            (my-filter odd? tl)
            (my-filter even? tl)))))

(out (same-parity 1 2 3 4 5 6 7)
     (same-parity 2 3 4 5 6 7))

(end-script)
