(load "../common/utils.scm")

(define nil '())

(define (square-list-1 items)
  (define (iter things answer)
    ; things -> rest things to do
    ; answer -> accumulated list for all answers done
    (if (null? things)
      answer
      (iter (cdr things)
	    ; here answer should be inserted after `answer`
	    (cons (square (car things))
		  answer))))
  (iter items nil))

(out (square-list-1 (list-in-range 1 10)))
; it works but the result is reversed

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
	    ; no work because a list cannot be constructed in this way
	    ; e.g. 
	    ; (cons 1 (cons 2 (cons 3 nil)))
	    ; is not equal to
	    ; (cons 1 (cons 2 (cons nil 3))) 
	    ; and the latter is not even a list
	    (cons answer
		  (square (car things))))))
  (iter items nil))

(out (square-list-2 (list-in-range 1 10)))
; answers are in order but the structure is weird

; here I have a corrected version
(define (square-list-correct items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
	    (append answer
		    (list (square (car things)))))))
  (iter items nil))

(out (square-list-correct (list-in-range 1 10)))

(end-script)
