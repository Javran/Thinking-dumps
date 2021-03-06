(import (rnrs))

(define (convert number)
  (define out
    (apply
     string-append
     (map (lambda (p)
            (if (zero? (remainder number (car p)))
                (cdr p)
                ""))
          '((3 . "Pling") (5 . "Plang") (7 . "Plong")))))
  (if (string=? out "")
      (number->string number)
      out))
