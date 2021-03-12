#lang racket

(require srfi/1 srfi/13)

(provide word-count)

(define (word-split sentence)
  (define stage-0
    ;; break string into consecutive chunk of chars
    ;; this is done by alternating `span` and `break` and only keep LHS of spans.
    (let ([keep? (lambda (ch) (or (eq? ch #\') (char-lower-case? ch) (char-numeric? ch)))])
      (let loop ([xs (string->list sentence)]
                 [rev-results '()])
        (let-values ([(xsa xsb) (span keep? xs)])
            (if (null? xsb)
                (reverse! (cons xsa rev-results))
                ;; further break down xsb
                (let-values ([(_xsba xsbb) (break keep? xsb)])
                  (loop xsbb (cons xsa rev-results))))))))
  (define (cleanup word)
    ;; remove surrounding quotes and empty words
    (let* ([l (string-length word)]
           [word-1
            (string-trim-both word (curry eq? #\'))])
      (if (= (string-length word-1) 0)
          #f
          word-1)))
  (define stage-1
    (filter-map (compose1 cleanup list->string) stage-0))
  stage-1)

(define (word-count sentence)
  (let ([m (make-hash)])
    (for-each
     (lambda (w)
       (hash-update! m w add1 0))
     (word-split (string-downcase sentence)))
    m))