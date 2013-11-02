(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define x1 '(a b c))
(out (count-pairs x1))

#|
return 3:
    +-+-+  +-+-+  +-+-+
x1->|.|.-->|.|.-->|.|/|
    +|+-+  +|+-+  +|+-+
     v      v      v
     a      b      c
|#

(define x2
  (let ()
    (define u '(c))
    (define v (cons 'b u))
    (cons u v)))
(out (count-pairs x2))

#|
return 4:
    +-+-+
x2->|.|.----+
    +|+-+   |
     |      |
     v      v
    +-+-+  +-+-+
    |.|.-->|.|/|
    +|+-+  +|+-+
     v      v
     b      c
|#

(define x3
  (let ()
    (define u '(c))
    (define v (cons u u))
    (cons v v)))
(out (count-pairs x3))

#|
return 7:
    +-+-+
x3->|.|.|
    +|+|+
     v v
    +-+-+  
    |.|.|
    +|+|+  
     v v   
    +-+-+
    |.|/|
    +|+-+
     v   
     c   
|#

(define x4
  (let ()
    (define u '(a b c))
    (set-cdr! (cddr u) u)
    u))

; (out (count-pairs x4))
; uncomment to run this test, will never terminate

#|
never return at all:
     +---------------+
     |               |
     v               |
    +-+-+  +-+-+  +-+|+
x4->|.|.-->|.|.-->|.|.|
    +|+-+  +|+-+  +|+-+
     v      v      v
     a      b      c
|#

(end-script)
