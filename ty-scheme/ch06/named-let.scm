(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

; only 'letrec' will work here
;     because 'let' cannot make countdown seen its definition in its body
(letrec (
  (countdown (lambda (i)
    (if (= i 0) 'liftoff
      (begin
	(out i)
	(countdown (- i 1)))))))
  (out (countdown 10)))

; from 10 to 1, and liftoff at last

; named-let works like loop-recur in clojure
; only 'let' works here, and 'letrec' & 'let*' will be regarded as ill forms
;     TODO: figure it out the reason
(out
  ; init: i should be 10
  (let countdown ((i 10))
    (if (= i 0) 'liftoff
      (begin
        (out i)
	; for next iteration, dec i by 1
        (countdown (- i 1))))))
