(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
  'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s  (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder as bs ss c)
  (define (ripple-carry-adder-intern as bs cn ss c)
    (if (= (length as) 1)
      (begin
        (assert (= (length bs) 1))
        (assert (= (length ss) 1))
        (full-adder (car as) (car bs) cn (car ss) c))
      (let ((c-mid (make-wire)))
        (ripple-carry-adder-intern (cdr as) (cdr bs) cn (cdr ss) c-mid)
        (full-adder (car as) (car bs) c-mid (car ss) c))))
  (let ((cn (make-wire)))
    (set-signal! cn 0)
    (ripple-carry-adder-intern as bs cn ss c)
    'ok))

#|
case 1: the last one
 
    |An   |Bn   |Cn
    v     v     v
  +-+-----+-----+-+
  |      FA       |
  +----+------+---- 
       |C     |Sn
       v      v

case 2: not the last one

                +-----------+
    |A1   |B1   |Cmid       |       |A2..An  |B2..Bn  |Cn
    v     v     v           |       v        v        v       
  +-+-----+-----+-+         |     +-+--------+--------+-+     
  |      FA       |         |     |  ripple-carry-adder |     
  +----+------+----         |     +----+------+---------+
       |C     |S1           |          |Cmid  |S2..Sn    
       v      v             +----------+      v        


|#

#|
What is the delay needed to obtain the complete output
  from an n-bit ripple-carry adder, expressed
  in terms of the delays for and-gates, or-gates, and inverters?

delay = total delay of n FAs
assume different gates are evaluated simultaneously,
one full-adder delay would be:
`2*half-adder-delay + or-gate-delay`
one half-adder delay would be:
`max(or-gate-delay, and-gate-delay + inverter-delay) + and-gate-delay`

the n-bit ripple-carry adder delay would be:
` 2n * max(or-gate-delay, and-gate-delay + inverter-delay)
+ 2n * and-gate-delay
+  n * or-gate-delay
`
|#

(load "./circuit_simulate.scm")
; test ripple-carry-adder
(define (int->bins n len)
  (if (= len 0)
    nil
    (append (int->bins (quotient n 2)
                       (- len 1))
            (list (remainder n 2)))))

(define (bins->int xs)
  (fold-left (lambda (acc i) (+ acc acc i))
             0
             xs))

(define (bin->wire b)
  (let ((x (make-wire)))
    (set-signal! x b)
    x))

; test a+b, length = n
(define (test-add a b n)
  (let* ((bins-a  (int->bins a n))
         (bins-b  (int->bins b n))
         (wire-c  (make-wire))
         (wires-a (map bin->wire bins-a))
         (wires-b (map bin->wire bins-b))
         (wires-n (map (lambda (x) (make-wire))
                       (list-in-range 1 n))))
    (out "constructing ripple-carry-adder ...")
    (ripple-carry-adder wires-a wires-b wires-n wire-c)
    (out "propagating ...")
    (propagate)
    (let ((result (bins->int (map get-signal wires-n))))
      (format #t "circuit output: ~A, carry: ~A~%output: ~A~%"
              result
              (get-signal wire-c)
              (+ a b)))))

(test-add 1234 5678 16)
; 6912
(test-add 65535 1 16)
; 65536 (overflow, output = 0 and carry = 1)

(end-script)
