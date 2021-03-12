#lang racket

(require srfi/13)

(provide score)

(define score-table
  '(("AEIOULNRST". 1)
    ("DG" . 2)
    ("BCMP" . 3)
    ("FHVWY". 4)
    ("K". 5)
    ("JX" . 8)
    ("QZ" . 10)))

(define (lookup ch)
  (let loop ([xs score-table])
    (cond
      [(null? xs) (raise 'invalid)]
      [(string-index (caar xs) ch) (cdar xs)]
      [else (loop (cdr xs))])))

(define (score word)
  (apply + (map lookup (string->list (string-upcase word)))))