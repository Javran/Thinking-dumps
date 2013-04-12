(load "../common/utils.scm")

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

; letrec

; lines below will not work, since 'local-odd?' cannot be seen outside
;     and 'let*' will not work for the same reason

; (let (
;   (local-even?
;     (lambda (n)
;       (if (= n 0) #t
;         (local-odd? (- n 1)))))
;   (local-odd?
;     (lambda (n)
;       (if (= n 0) #f
;         (local-odd? (- n 1))))))
;   (list (local-even? 23) (local-odd? 23)))

; 'letrec' make indroduction visible in letrec body & all initialization
(letrec (
  (local-even?
    (lambda (n)
       (if (= n 0) #t
         (local-odd? (- n 1)))))
   (local-odd?
     (lambda (n)
       (if (= n 0) #f
         (local-odd? (- n 1))))))
   (out (list (local-even? 23) (local-odd? 23))))
; '(#f #f)
