#lang racket

(require srfi/13)
(provide grep)

(define text-file?
  ;; an abstract text file is ...
  (cons/c
   ;; file name
   string?
   ;; contents
   (listof
    ;; lines paired with line numbers.
    (cons/c integer? string?))))

(define/contract (read-text-file path)
  (string? . -> . text-file?)
  (cons
   path
   (call-with-input-file path
     (lambda (in)
       (let loop ([lines-rev '()]
                  [num 1])
         (let ([content (read-line in)])
           (if (eof-object? content)
               (reverse lines-rev)
               (loop (cons (cons num content) lines-rev)
                     (+ num 1))))))
     #:mode 'text)))

(define/contract (make-line-matcher flags pattern)
  ;; create a function that matches against a line
  ((listof string?) string? . -> . (string? . -> . any/c))
  (let ([ci? (member "-i" flags)]
        [invert? (member "-v" flags)]
        [entire? (member "-x" flags)])
    (let ([string-match
           (if entire?
               (if ci? string-ci=? string=?)
               (if ci? string-contains-ci string-contains))])
      (lambda (content)
        (let ([result
               (string-match content pattern)])
          (if invert?
              (not result)
              result))))))

(define/contract (make-outputer flags multi-files?)
  ((listof string?)
   boolean?
   . -> .
   (text-file? . -> . (listof string?)))
  (match 
      (filter (lambda (f) (member f '("-n" "-l"))) flags)
    ['()
     ;; output just lines in normal mode
     (match-lambda
       [(cons fname lines)
        (map
         (compose1
          (if multi-files?
              (lambda (xs) (~a fname #\: xs))
              identity) cdr)
         lines)])]
    ;; checking -l first and then -n to deal with an interaction:
    ;; if both -l and -n are present,
    ;; this is the same as just -l, since -n only acts on lines.
    [_ #:when (member "-l" flags)
       ;; file names only mode
       (compose1 list car)]    
    [_ #:when (member "-n" flags)
       ;; output with line numbers
       (match-lambda
         [(cons fname lines)
          (map
           (compose1
            (if multi-files?
                (lambda (xs) (~a fname #\: xs))
                identity)
            (match-lambda [(cons l xs) (~a l #\: xs)]))
           lines)])]))

(define (grep flags-pre pattern files)
  (let* ([flags (remove-duplicates flags-pre)]
         [line-matcher
          (make-line-matcher flags pattern)]
         [multi-files?
          (match files
            ['() #f]
            [(list _) #f]
            [_ #t])]
         [outputer
          (make-outputer flags multi-files?)])
    (define/contract (file-filter tf)
      (text-file? . -> . (first-or/c #f text-file?))
      (match-let ([(cons fn lines) tf])
        (let ([filtered-lines
               (filter (compose1 line-matcher cdr) lines)])
            (and (not (null? filtered-lines))
                 (cons fn filtered-lines)))))
    (append-map
     outputer
     (filter-map
      (compose1 file-filter read-text-file)
      files))))