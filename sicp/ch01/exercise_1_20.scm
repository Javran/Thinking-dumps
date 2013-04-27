(load "../common/utils.scm")

(define (my-gcd a b)
  (if (= b 0)
    a
    (my-gcd b (remainder a b))))

; normal order:
; (gcd 206 40)
;
; 'if' evaluates its condition: 
;   (= 40 0) => #f
; remainder call counter: 0
; 
; (gcd 40 (remainder 206 40)) 
; 'if' evaluates its condition:
;   (= (remainder 206 40) 0) => #f
; remainder call counter: 0+1
;
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; 'if' evaluates its condition:
;   (= (remainder 40 (remainder 206 40)) 0) => #f
; remainder call counter: 1+2
;
; (gcd (remainder 40 (remainder 206 40)) 
;      (remainder (remainder 206 40)
;                 (remainder 40 (remainder 206 40))))
; 'if' evaluates its condition:
; (= (remainder (remainder 206 40)
;               (remainder 40 (remainder 206 40))) 0) => #f
; remainder call counter: 3+4
;
; (gcd (remainder (remainder 206 40)
;                 (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40)) 
;                 (remainder (remainder 206 40)
;                            (remainder 40 (remainder 206 40)))))
; 'if' evaluates its condition:
; (= (remainder (remainder 40 (remainder 206 40)) 
;               (remainder (remainder 206 40)
;                          (remainder 40 (remainder 206 40)))) 0) => #t
; remainder call counter: 7+7
;
; (remainder (remainder 206 40)
;            (remainder 40 (remainder 206 40)))
;
; everything is primitive, we can full evaluate it now
; 2
; remainder call counter: 14+4
; final 'remainder' call counter for normal-order evaluation: 18

; applicative order:
; (gcd 206 40)
; (= 40 0) => #f
; (gcd 40 (remainder 206 40))
; force evaluation of the last element:
; (remainder 206 40)
; remainder call counter: 0+1
;
; (gcd 40 6)
; (= 6 0) => #f
; (gcd 6 (remainder 40 6))
; force evaluation of the last element:
; (remainder 40 6)
; remainder call counter: 1+1
;
; (gcd 6 4)
; (= 4 0) => #f
; (gcd 4 (remainder 6 4))
; force evaluation of the last element:
; (remainder 6 4)
; remainder call counter: 2+1
;
; (gcd 4 2)
; (= 2 0) => #f
; (gcd 2 (remainder 4 2))
; force evaluation of the last element:
; (remainder 4 2)
; remainder call counter: 3+1
;
; (gcd 2 0)
; (= 0 0) => #t
; 2
; final 'remainder' call counter for applicative-order evaluation: 4
