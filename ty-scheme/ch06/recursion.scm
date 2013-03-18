(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(define factorial
  (lambda (n)
    (if (= n 0) 1
      (* n (factorial (- n 1))))))

(out (factorial 10))
; 3628800

; I'd like to ask why can mutually recursive work,
;     given that the time when is-even? is defined,
;     it have no idea about what is 'is-odd?'
(define is-even?
  (lambda (n)
    (if (= n 0) #t
      ; why can is-even see is-odd???
      (is-odd? (- n 1)))))

(define is-odd?
  (lambda (n)
    (if (= n 0) #f
      (is-even? (- n 1)))))

(out (is-even? 10))
; #t
(out (is-odd? 9))
; #t
