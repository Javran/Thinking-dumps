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

(end-script)
