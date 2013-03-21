(load "../common/utils.scm")

; just follow the suggestion here:
; http://stackoverflow.com/questions/15552057/is-it-possible-to-implement-define-macro-in-mit-scheme/
; we should prefer syntax-rules over define-macro
;     so that we'll get clearer codes.
(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((name . pattern) template))))))

(define-syntax-rule (my-when test branch)
  (if test
    (begin branch)))

(my-when #t
  (display "This string should be printed"))

(my-when #f
  (display "This string should not be printed"))

