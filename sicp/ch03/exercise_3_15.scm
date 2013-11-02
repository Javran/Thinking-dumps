(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

#|

    +-+-+
z1->|.|.|
    +|+|+
     v v
    +-+-+  +-+-+
x ->|.|.-->|.|/|
    +|+-+  +|+-+
     v      v
     a      b

    +-+-+  +-+-+  +-+-+
z2->|.|.-->|.|.-->|.|/|
    +|+-+  +|+-+  +|+-+
     |      v      v
     |      a      b
     |      ^      ^
     |     +|+-+  +|+-+
     +---->|.|.-->|.|/|
           +-+-+  +-+-+
    
after applying `set-to-wow!`:

    +-+-+
z1->|.|.|
    +|+|+
     v v
    +-+-+  +-+-+
x ->|.|.-->|.|/|
    +|+-+  +|+-+
     v      v
    wow     b

    +-+-+  +-+-+  +-+-+
z2->|.|.-->|.|.-->|.|/|
    +|+-+  +|+-+  +|+-+
     |      v      v
     |      a      b
     |             ^
     |     +-+-+  +|+-+
     +---->|.|.-->|.|/|
           +|+-+  +-+-+
            v
           wow
|#

(out z1 z2)
; ((a b) a b)
; ((a b) a b)
(set-to-wow! z1)
(set-to-wow! z2)

(out z1 z2)
; ((wow b) wow b)
; ((wow b) a b)

(end-script)
