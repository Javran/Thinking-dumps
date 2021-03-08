(import (rnrs))

(use-modules ((ice-9 match)))

(define (attacking? white black)
  (match
   (cons white black)
   [((r0 f0) . (r1 f1))
    (or (= r0 r1)
        (= f0 f1)
        (= (+ r0 f0) (+ r1 f1))
        (= (- r0 f0) (- r1 f1)))]))

