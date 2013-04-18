(load "../common/utils.scm")

(define (factorial-1 n)
  (if (= n 0)
    ; 0! = 1
    1
    ; we can compute n! by computing (n - 1)! and multiplying the result by n
    (* n (factorial-1 (- n 1)))))

(define test-factorial
  (lambda (factorial)
    (let loop ((i 0))
      (if (< i 10)
        (begin
          (display i)
          (display "! =\t")
          (out (factorial i))
          (loop (+ i 1))))))
  ; print result of 0! to 9!
  )

(test-factorial factorial-1)

; another prospective: multiply 1 by 2, multiply the result by 3 .. until we reach n
(define (factorial-2 n)
  (define (factorial-iter product counter max-count)
    ; counter is what we want to multple in next
    (if (> counter max-count)
      ; we don't need to multiple the product by counter, return it as result
      product
      (factorial-iter
        (* counter product)
        (+ counter 1)
        max-count)))
  (factorial-iter 1 1 n))

(test-factorial factorial-2)

; recursive process: 
; * characterized by a chain of deferred operations (like factorial-1)
; * requires that the interpreter keep track of the operations to be performed later on

; linear recursive process:
; * the amount of information needed to keep track of it,
;   grows linearly with n (is proportional to n)

; iterative process:
; * does not grow and shrink
; * one whose state can be summarized by a fixed number of state variables

; linear iterative process:
; * the number of steps required grows linearly with n.

; actually I don't think these concepts are useful ... waiting for further explanation.
