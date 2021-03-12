#lang racket

(provide add-gigasecond)

(require racket/date)

(define add-gigasecond
  (compose1
   seconds->date
   (curry + 1e9)
   date*->seconds))