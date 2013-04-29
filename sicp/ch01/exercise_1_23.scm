(load "../common/utils.scm")

(define (next x)
  (if (= x 2)
    3
    (+ x 2)))

(out "test 'next' with init seed 2:")
(out (take-iterate next 2 5))
; should be (2 3 5 7 9)

; here we need to keep track of computation performed
;     since the speed is too fast to infer duration

; modified version of smallest-divisor
;     which returns a pair: (result . computation performed)
(define (tracked-smallest-divisor n next)
  (define (divides? a b)
    ; consumption: 2: =, remainder
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) 
            ; time consumption: condition eval: square + `>` = 2
            ; cons +1
            (cons n 3)) ; impossible
          ((divides? test-divisor n) 
            ; time consumption: prev cond eval(2) + current cond(2) = 4
            ; cons +1
            (cons test-divisor 5))
          (else 
            ; time consumption: prev cond eval(4)
            (let ((result (find-divisor n (next test-divisor))))
              (cons (car result)
                    ; car, cdr, +, cons: 4
                    (+ (cdr result) 4))))))
  (find-divisor n 2))

; pass counter to other functions
(define (tracked-prime? n next)
  (let ((result (tracked-smallest-divisor n next)))
    (cons (= (car result) n) (cdr result))))

(define (original-next x)
  (+ x 1))

(define (verbose-prime-test n)
  (let ((ori-result (tracked-prime? n original-next))
        (opt-result (tracked-prime? n next)))
    (display "input: ")
    (display n)
    (newline)
    (display "original-next output : ")
    (display ori-result)
    (newline)
    (display "optimized-next output: ")
    (display opt-result)
    (newline)
    (display "speed ratio: (ori/opt) = ")
    (display (exact->inexact 
               (/ (cdr ori-result)
                  (cdr opt-result))))
    (newline)))

(verbose-prime-test 100000007)
; (#t . 39999)
; (#t . 20003)

; for number 100000007, the time consumption is roughly as expected
; let's test other cases
(for-each
  verbose-prime-test
  '(    1009    1013    1019
       10007   10009   10037
      100003  100019  100043
     1000003 1000033 1000037))
; all speed ratio should be close to 2
