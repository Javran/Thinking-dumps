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

;(out #1
  (+ 1
     (call/cc
       (lambda (k)
         (set! r k)
         (+ 2 (k 3)))))

;) #2

; 4

; r seems to be called "escaping procedure":
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Continuations.html

(out 1)
(out (r 5))
; 6
(out 2)
(out (+ 3 (r 5)))
; 6

; if you uncomment #1 and #2, you'll be able to see:
;     everything is aborted except "out"
; the answer is:
; if we comment out #1 and #2, nothing will be outputed
; so actually we jump to the statement (out (+ 1 [])) with [] filled with the given argument!

(out 3)
(out
  (r 5)
  (+ 3 (r 5)))
; 6

(newline)

; so let's test again:
(define aaa
  (+ 1 (call/cc
         (lambda (k)
           (set! r k)
           (+ 2 (k 3))))))

(out aaa)
; 4
(r 1)
; nothing is printed
(out aaa)
; but aaa got redefined
; 2
(out (r 2))
; nothing is printed
(out aaa)
; but aaa got redefined
; 3

; some page that might be useful:
; http://community.schemewiki.org/?call-with-current-continuation
; https://en.wikipedia.org/wiki/Call_with_current_continuation

(define redo1 #f)
(define redo2 #f)
(define redo3 #f)

(out
  (call/cc
    (lambda (c)
      (set! redo1 c)
      (c "First line")))
  (call/cc
    (lambda (c)
      (set! redo2 c)
      (c "Second line")))
  (call/cc
    (lambda (c)
      (set! redo3 c)
      (c "Third line"))))

; this might be a good example to understand "current continuation"
; think about it:
; * why "redo2" still shows "First line"
; * why "redo3" still shows "First line" & "Second line"

(redo1 "Modified first line!")
; Modified first line!
; Second line
; Third line

(redo2 "Modified second line!")
; First line
; Modified second line!
; Third line

(redo3 "Modified third line!")
; First line
; Second line
; Modified third line!
