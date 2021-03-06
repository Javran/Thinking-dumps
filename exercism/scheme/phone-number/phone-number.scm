(import (rnrs))

(use-modules ((srfi srfi-1) #:select (filter-map)))
(use-modules ((srfi srfi-13)))

(define (clean-char ch)
  (cond
   [(char-whitespace? ch) #f]
   [(string-index ".()+-" ch) #f]
   [(char-numeric? ch) ch]
   [else (raise 'invalid)]))

(define (clean phone-number)
  (let* ([all-digits
          (filter-map clean-char (string->list phone-number))]
         [l (length all-digits)]
         [digits
          (cond
           [(= l 11)
            (if (eq? (car all-digits) #\1)
                (list->string (cdr all-digits))
                (raise 'invalid))]
           [(= l 10) (list->string all-digits)]
           [else (raise 'invalid)])])
    (if (and (char<=? #\2 (string-ref digits 0) #\9)
             (char<=? #\2 (string-ref digits 3) #\9))
        digits
        (raise 'invalid))))
