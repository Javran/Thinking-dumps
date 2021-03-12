#lang racket

(provide armstrong-number?)

(define (armstrong-number? n)
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