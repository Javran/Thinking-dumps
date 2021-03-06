(import (rnrs))

(use-modules ((srfi srfi-1) #:select (span break)))
(use-modules ((srfi srfi-69)))

(define (word-split sentence)
  (define (keep? x)
    (or (eq? x #\') (char-lower-case? x)))
  (let loop ([xs (string->list sentence)]
             [rev-results '()])
    (call-with-values
        (lambda ()
          (span keep? xs))
      (lambda (xsa xsb)
        (if (null? xsb)
            (reverse! (cons xsa rev-results))
            ;; further break down xsb
            (call-with-values
                (lambda ()
                  (break keep? xsb))
              (lambda (_xsba xsbb)
                (loop xsbb (cons xsa rev-results)))))))))

(define (word-count sentence)
  (word-split (string-downcase sentence)))

(if #t
    (begin
      (display
       (word-count
        (string-append
         "Joe can't tell between app, apple \n\n\n \n and a."
         "Joe can't tell between 'large' and large."
         "First: don't laugh. Then: don't cry. a.a.a."
         "car: carpet as java: javascript!!&@$%^& ")))
      (newline)))
