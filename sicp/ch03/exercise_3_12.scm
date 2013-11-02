(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (append! x y)
  (define (last-pair x)
    (if (null? (cdr x)) x (last-pair (cdr x))))
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

#|
x:
+-+-+  +-+-+
|.|.-->|.|/|
+|+-+  +|+-+
 v      v
 a      b

y:
+-+-+  +-+-+
|.|.-->|.|/|
+|+-+  +|+-+
 v      v
 c      d

z:
+-+-+  +-+-+  +-+-+  +-+-+
|.|.-->|.|.-->|.|.-->|.|/|
+|+-+  +|+-+  +|+-+  +|+-+
 v      v      v      v
 a      b      c      d

|#

(out z)
; (a b c d)

(out (cdr x))
; (b)

(define w (append! x y))

#|
x:
+-+-+  +-+-+
|.|.-->|.|.|
+|+-+  +|+|+
 v      v |
 a      b |
          |
y:        |
 +--------+
 v
+-+-+  +-+-+
|.|.-->|.|/|
+|+-+  +|+-+
 v      v
 c      d

w:
+-+-+  +-+-+  +-+-+  +-+-+
|.|.-->|.|.-->|.|.-->|.|/|
+|+-+  +|+-+  +|+-+  +|+-+
 v      v      v      v
 a      b      c      d

|#

(out w)
; (a b c d)

(out (cdr x))
; (b c d)

(end-script)
