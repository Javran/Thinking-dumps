#lang racket
(provide my-reverse)

(define my-reverse
  (compose1
   list->string
   reverse
   string->list))