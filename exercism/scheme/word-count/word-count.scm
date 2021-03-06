(import (rnrs))

(use-modules ((srfi srfi-1) #:select (span break filter-map)))
(use-modules ((srfi srfi-13)))

(define (word-split sentence)
  (define stage-0
    ;; break string into consecutive chunk of chars
    ;; this is done by alternating `span` and `break` and only keep LHS of spans.
    (let ([keep? (lambda (ch) (or (eq? ch #\') (char-lower-case? ch) (char-numeric? ch)))])
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
                    (loop xsbb (cons xsa rev-results))))))))))
  (define (cleanup word)
    ;; remove surrounding quotes and empty words
    (let* ([l (string-length word)]
           [word-1
            (string-trim-both word (lambda (ch) (eq? ch #\')))])
      (if (= (string-length word-1) 0)
          #f
          word-1)))
  (define stage-1
    (filter-map (lambda (x) (cleanup (list->string x))) stage-0))
  stage-1)

(define (word-count sentence)
  (define m (make-hashtable string-hash equal?))
  (for-each
   (lambda (w)
     (hashtable-update! m w (lambda (x) (+ x 1)) 0))
   (word-split (string-downcase sentence)))
  m)
