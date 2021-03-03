(import (rnrs))

(use-modules (ice-9 match))

(define (anagram target words)
  (define (downcase-and-sort s)
    (let* ([s-dc (string-downcase s)]
           [s-norm (list->string
                    (sort-list!
                     (string->list s-dc) char<?))])
      (cons s-dc s-norm)))
  (match
   (downcase-and-sort target)
   [(t-dc . t-norm)
    (filter (lambda (w)
              (match
               (downcase-and-sort w)
               [(w-dc . w-norm)
                (and (not (equal? t-dc w-dc))
                     (equal? t-norm w-norm))])) words)]))
