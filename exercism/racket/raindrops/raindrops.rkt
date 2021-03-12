#lang racket

(provide convert)

(define (convert number)
  (define out
    (apply
     string-append
     (map (match-lambda
            [(cons a b)
             (if (zero? (remainder number a))
                 b
                 "")])
          '((3 . "Pling") (5 . "Plang") (7 . "Plong")))))
  (if (string=? out "")
      (number->string number)
      out))