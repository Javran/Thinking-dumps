(load "../common/utils.scm")

(define list-position
  (lambda (obj ls)
    (let loop (
      (ind 0) ; index = 0
      (current-list ls); list
      )
    (if (null? current-list) #f ; nothing left, return false
      (if (eqv? (car current-list) obj) ind ; the head of current-list = obj, return index
	(loop (+ ind 1) (cdr current-list))))))) ; else, ind -> +1, current-list -> cdr

(out (list-position 2 '(1 2 3 4 5)))
; 1
(out (list-position 0 '(1 2 3 4 5)))
; #f

; seems here 'reverse!' is more efficient than 'reverse' ...
(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
	  (let ((d (cdr s)))
            (set-cdr! s r)
	    (loop d s))))))

(define test-list '(1 2 3 4 5))
(out test-list)
(out (reverse! test-list))
(out test-list)
; note "reverse!" tends to be DESTRUCTIVE:
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Miscellaneous-List-Operations.html#Miscellaneous-List-Operations
; the comment practice is to use it with set! 
; i.e. : (set! x (reverse! x))

(define get-reverse
  (lambda (seq)
    ; the thing we need to take into account is:
    ; what to reverse? & things that have been reversed
    (let loop ((rest-seq seq) (result '()))
       (if (null? rest-seq) result ; nothing need to reverse, return the result
         ; break rest-seq into head & tail
         (let ((head (car rest-seq)) (tail (cdr rest-seq)))
	   ; for tail, we'll set it as the rest things and loop;
	   ; for head, we'll append it to the head of result
           (loop tail (cons head result)))))))

(define test-list '(1 2 3 4 5))
; what we need is a list as input
(out test-list)
(out (get-reverse test-list))
; output should be:
; '(5 4 3 2 1)
(out test-list)
; while the inputted list keep unchanged
; '(1 2 3 4 5)
