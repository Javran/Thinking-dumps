;; transform variables to make it easier for processing

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

;; search symbols recursively and apply "proc" to it
(define (map-over-symbols proc exp)
  (cond ((non-empty? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp)
         (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (test-expand-question-mark)
  (do-test
   expand-question-mark
   (list
    (mat '?test '(? test))
    (mat 'whatever 'whatever)
    (mat '?symbol '(? symbol)))))

(define (test-query-syntax-process)
  (do-test
   query-syntax-process
   (list
    (mat '(job ?x ?y) '(job (? x) (? y)))
    (mat '?symbol '(? symbol))
    (mat '(job x y) '(job x y)))))

(if *qeval-tests*
    (begin
      (test-expand-question-mark)
      (test-query-syntax-process))
    'ok)

;; local variables:
;; proc-entry: "./qeval.scm"
;; End:
