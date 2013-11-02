(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; I guess this will run into an infinite list
; define an auxiliary function to guaranteed that
; this code can be terminated.

#|
illustration:

the list passed into `make-cycle`:
+-+-+  +-+-+  +-+-+
|.|.-->|.|.-->|.|/|
+|+-+  +|+-+  +|+-+
 v      v      v
 a      b      c

and after:
 +---------------+
 |               |
 v               |
+-+-+  +-+-+  +-+|+
|.|.-->|.|.-->|.|.|
+|+-+  +|+-+  +|+-+
 v      v      v
 a      b      c

|#

(define (take n ls)
  (if (or (<= n 0) (null? ls))
    nil
    (cons (car ls) (take (- n 1) (cdr ls)))))

(define z (make-cycle (list 'a 'b 'c)))

(out (take 100 z))

; uncomment to observe the infinite loop
; (last-pair z)

(end-script)
