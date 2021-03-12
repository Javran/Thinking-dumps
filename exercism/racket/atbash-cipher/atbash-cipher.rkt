#lang racket

(provide encode decode)

(define (destruct-char
         when-lower
         when-digit
         when-space
         otherwise)
  (lambda (ch)
    (cond
     [(char=? ch #\space) when-space]
     [(char-lower-case? ch)
      (when-lower ch)]
     [(char-numeric? ch)
      (when-digit ch)]
     [else (otherwise ch)])))

(define int-over-lower-char
  ;; allow a function to operate a lowercase char as if it's an int in 0..25
  (let ([base (char->integer #\a)])
    (lambda (modify)
      (lambda (ch)
        ;; assuming ch is a lowercase char
        (let ([val (- (char->integer ch) base)])
          (integer->char (+ (modify val) base)))))))

(define (group-encoded chars)
  ;; takes a list of chars and turns it into a string of space-separated
  ;; groups, in which each group consists of 5 characters.
  (let loop ([result ""]
             [xs chars])
    (define (pack-new-item cs)
      (if (null? cs)
          result
          ;; pack cs and generate a new result string, taking into account spaces.
          (let ([packed (list->string cs)])
            (if (string=? result "")
                packed
                (string-append result " " packed)))))
    (if (< (length xs) 5)
        (pack-new-item xs)
        (call-with-values (lambda () (split-at xs 5))
          (lambda (ys zs)
            (loop (pack-new-item ys) zs))))))

(define (make-encode-or-decode finalizer)
  (lambda (xs)
    (finalizer
     (filter-map
      (destruct-char
       ;; when-lower
       (int-over-lower-char
        (lambda (x) (- 25 x)))
       ;; when-digit
       (lambda (x) x)
       ;; when-space
       #f
       ;; otherwise
       (lambda (_) #f))
      (string->list (string-downcase xs))))))

(define encode (make-encode-or-decode group-encoded))
(define decode (make-encode-or-decode list->string))