#lang racket

(require srfi/13)

(provide nanp-clean)

(define (err)
  (raise (error 'invalid)))

(define (clean-char ch)
  (cond
   [(char-whitespace? ch) #f]
   [(string-index ".()+-" ch) #f]
   [(char-numeric? ch) ch]
   [else (err)]))

(define (nanp-clean phone-number)
  (let* ([all-digits
          (filter-map clean-char (string->list phone-number))]
         [l (length all-digits)]
         [digits
          (cond
           [(= l 11)
            (if (eq? (car all-digits) #\1)
                (list->string (cdr all-digits))
                (err))]
           [(= l 10) (list->string all-digits)]
           [else (err)])])
    (if (and (char<=? #\2 (string-ref digits 0) #\9)
             (char<=? #\2 (string-ref digits 3) #\9))
        digits
        (err))))