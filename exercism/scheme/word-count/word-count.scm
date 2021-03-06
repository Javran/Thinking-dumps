(import (rnrs))

(use-modules ((srfi srfi-1) #:select (span break filter-map)))

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
            (if (and (>= l 2)
                     (eq? (string-ref word 0) #\')
                     (eq? (string-ref word (- l 1)) #\'))
                (string-copy word 1 (- l 1))
                word)])
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
