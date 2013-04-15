(load "../common/utils.scm")

; to calculate the absolute value of a number:
; (abs r) = r  (if r > 0)
;         = 0  (if r = 0)
;         = -r (if r < 0)
; this construct is called a *case analysis*

(define (test-abs abs-to-be-tested)
  (let ((testcase '(1 2 -1 -0.5 0 10)))
    (let ((supposed-result (map abs testcase))
          (actual-result (map abs-to-be-tested testcase)))
      ; http://sicp.ai.mit.edu/Fall-2003/manuals/scheme-7.5.5/doc/scheme_4.html
      (equal? supposed-result actual-result))))

; not to conflict with one in standard lib
(define (my-abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(out (test-abs my-abs))
; #t if test passed

; the general form is:
; (cond (<p1> <e1>)
;       (<p2> <e2>)
;       ...
;       (<pn> <en>))
; the return value of "cond" is not defined if all <px> are not met


(define (my-abs-1 x)
  (cond ((< x 0) (- x))
        (else x)))

(out (test-abs my-abs-1))
; #t if test passed

(out (test-abs (lambda (x) (if (< x 0) (- x) x))))
; #t if test passed

; if expression:
; (if <predicate> <consequent> <alternative>)

; logical composition:
; (and <e1>...<en>)
; (or <e1>...<en>)
; (not <e>)

(out (and #t #t #f))
; #f
(out (and #t #t))
; #t
(out (or #f (and #t (not #f))))
; #t
