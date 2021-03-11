#lang racket

(provide anagrams-for)

(define (downcase-and-sort s)
  ;; provides downcased string and downcased+sorted string
  ;; for anagram tests.
  (let* ([s-dc (string-downcase s)]
         [s-norm (list->string
                  (sort
                   (string->list s-dc) char<?))])
    (cons s-dc s-norm)))

(define (anagrams-for target words)
  (match (downcase-and-sort target)
    [`(,t-dc . ,t-norm)
     (filter (lambda (w)
               (match (downcase-and-sort w)
                 [`(,w-dc . ,w-norm)
                  (and (not (equal? t-dc w-dc))
                       (equal? t-norm w-norm))])) words)]))