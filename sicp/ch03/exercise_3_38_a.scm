(load "../common/utils.scm")
(load "../common/test-utils.scm")

#|

Pater -> A
Paul  -> B
Mary  -> C

to get all different possible values,
let try to make things happen in all possible
permutations.

#1: ABC

100 + 10 = 110
110 - 20 = 90
90 - 45 = 45

#2: ACB

100 + 10 = 110
110 - 55 = 55
55 - 20 = 35

#3: BAC

100 - 20 = 80
80 + 10 = 90
90 - 45 = 45

#4: BCA

100 - 20 = 80
80 - 40 = 40
40 + 10 = 50

#5: CAB

100 - 50 = 50
50 + 10 = 60
60 - 20 = 40

#6: CBA

100 - 50 = 50
50 - 20 = 30
30 + 10 = 40

|#

; or we can demonstrate that through functions:
(define (A balance) (+ balance 10))
(define (B balance) (- balance 20))
(define (C balance) (- balance (/ balance 2)))

(define (perform-in-order procs)
  ((apply compose (reverse procs)) 100))

(out (map
       perform-in-order
       (list (list A B C)
             (list A C B)
             (list B A C)
             (list B C A)
             (list C A B)
             (list C B A))))

; all possible values: 35, 40, 45, 50
; p.s. there will be only one possible value if Mary is not that weird...

(end-script)
