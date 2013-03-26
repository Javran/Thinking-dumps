(load "../common/utils.scm")

(out
  (+ 1 
    (call/cc
      (lambda (k)
        (+ 2 (k 3))))))

; for (call/cc ...) part, the rest part is "(+ 1 [])"
;     here "[]" is used for place holder
; I think the rest part "(+ 1 [])" is "the current continuation"
;     and is bound to "k" in "(lambda (k) ...)"
; the computation "(+ 2 ...)" is aborted, and replaced with "(k 3)"
;     so the final result is "(+ 1 3)", i.e. 4

(define r #f)

(out
  (+ 1
     (call/cc
       (lambda (k)
         (set! r k)
         (+ 2 (k 3))))))
; 4

; r seems to be called "escaping procedure":
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Continuations.html

(out 1)
(out (r 5))
; 6
(out 2)
(out (+ 3 (r 5)))
; 6

; why everything is aborted except "out"?
; why can we still see the output?
(out 3)
(out
  (r 5)
  (+ 3 (r 5)))
; 6
