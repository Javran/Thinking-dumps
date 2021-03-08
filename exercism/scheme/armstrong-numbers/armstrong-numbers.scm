(import (rnrs))

(define (armstrong-numbers? n)
  (let* ([str (number->string n)]
         [ds (string->list str)]
         [l (string-length str)])
    (= n
       (apply + (map (lambda (ch)
                       (expt
                        (- (char->integer ch)
                           (char->integer #\0))
                        l))
                     ds)))))

(define armstrong-number? armstrong-numbers?)
