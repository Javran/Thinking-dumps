(load "../../common/guile/defstruct.scm")
(load "../../common/guile/alist.scm")

(define *form-data-table* (make-table 'equ string=?))

(define hex->char
  (lambda (x y)
    (integer->char
      (string->number (string x y) 16))))

(define url-decode
  (lambda (str)
    (let ((s (string->list str)))
      (list->string
        (let loop ((s s))
          (if (null? s)
            '()
            ; else
            (let ((head (car s))
                  (tail (cdr s)))
              (case head
                ((#\+)
                 ; '+' -> ' '
                 (cons #\space (loop tail)))
                ((#\%)
                 ; "%??" -> char
                 (cons (hex->char (car tail) (cadr tail))
                             (loop (cddr tail))))
                (else (cons head (loop tail)))))))))))

; break string s into substrings, the seperator is c
(define split
  (lambda (c s)
    (let loop ((s s))
      (if (string=? s "")
        ; nothing to do, return empty list
        '()
        ; elsewise
        (let ((i (string-index s c)))
          (if i
            ; found
            (cons (substring s 0 i)
                  (loop (substring s (+ i 1) (string-length s))))
            ; else, no more seperator, let s be the last element
            (list s)))))))

(define form-data-get
  (lambda (k)
    (table-get *form-data-table* k '())))

(define form-data-get/1
  (lambda (k . default)
    (let ((vv (form-data-get k)))
      (cond
        ((pair? vv) (car vv)) ; not an empty list -> get first elem
        ((pair? default) (car default))
        (else "")))))

(define parse-form-data-using-query-string
  (lambda ()
    (let ((query-string (or (getenv "QUERY_STRING") "")))
      (for-each
        (lambda (par=arg)
          (let ((par/arg (split #\= par=arg)))
            (let ((par (url-decode (car par/arg)))
                  (arg (url-decode (cadr par/arg))))
              (table-put!
                *form-data-table* par
                (cons arg
                      (table-get *form-data-table* par '()))))))
        (split #\& query-string)))))

(define parse-form-data
  (lambda ()
    ((if (string-ci=? (or (getenv "REQUEST_METHOD") "GET") "GET")
       ; GET
       parse-form-data-using-query-string
       ; POST
       parse-form-data-using-stdin))))

; I don't think the code here is friendly enough to be deciphered.
; just copy things here, TODO: future deciphering
(define parse-form-data-using-stdin
  (lambda ()
    (let* ((content-length (getenv "CONTENT_LENGTH"))
           (content-length
             (if content-length
               (string->number content-length) 0))
           (i 0))
      (let par-loop ((par '()))
        (let ((c (read-char)))
          (set! i (+ i 1))
          (if (or (> i content-length)
                  (eof-object? c) (char=? c #\=))
            (let arg-loop ((arg '()))
              (let ((c (read-char)))
                (set! i (+ i 1))
                (if (or (> i content-length)
                        (eof-object? c) (char=? c #\&))
                  (let ((par (url-decode
                               (list->string
                                 (reverse! par))))
                        (arg (url-decode
                               (list->string
                                 (reverse! arg)))))
                    (table-put! *form-data-table* par
                                (cons arg (table-get *form-data-table*
                                                     par '())))
                    (unless (or (> i content-length)
                                (eof-object? c))
                      (par-loop '())))
                  (arg-loop (cons c arg)))))
            (par-loop (cons c par))))))))

(define-macro unless
  (lambda (test . branch)
    (list 'if
          (list 'not test)
          (cons 'begin branch))))

; display html-encoded s to port o, if o is not given, stdout is selected.
(define display-html
  (lambda (s . o)
    (let ((o (if (null? o) (current-output-port)
                 (car o))))
      (let ((n (string-length s)))
        (let loop ((i 0))
          (unless (>= i n)
            (let ((c (string-ref s i)))
              (display
               (case c
                 ((#\<) "&lt;")
                 ((#\>) "&gt;")
                 ((#\") "&quot;")
                 ((#\&) "&amp;")
                 (else c)) o)
              (loop (+ i 1)))))))))
