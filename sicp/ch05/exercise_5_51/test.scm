(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map f (cdr xs)))))

(define (add1 x)
  (+ x 1))

(display (map add1 '(1 2 3 4 5)))
(newline)

(map
 (lambda (x)
   (display x)(newline))
 '("this" "is" "a" "test"))

(define (assert-thunk thunk mesg)
  (if (thunk)
      #t
      (error mesg)))

(assert-thunk
 (lambda ()
   #t)
 "true")

(assert-thunk
 (lambda ()
   (not #f))
 "false")
