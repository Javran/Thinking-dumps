(import (rnrs))

(use-modules ((srfi srfi-13)))

(define (get-base ch)
  (if (char-alphabetic? ch)
      (char->integer (if (char-upper-case? ch) #\A #\a))
      #f))

(define (over-int f)
  (lambda (ch)
    (cond
     [(get-base ch) =>
      (lambda (base)
        (integer->char (+ base (f (- (char->integer ch) base)))))]
     [else ch])))

(define (rotate phrase dx)
  (string-map
   (over-int (lambda (v) (modulo (+ v dx) 26)))
   phrase))
