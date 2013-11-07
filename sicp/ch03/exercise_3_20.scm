(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (my-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))

  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)))
    dispatch)

(define (my-car a) (a 'car))
(define (my-cdr a) (a 'cdr))

(define (my-set-car! a v)
  ((a 'set-car!) v) v)

(define (my-set-cdr! a v)
  ((a 'set-cdr!) v) v)


#|

G: (parent = nil)
  my-cons = ...
  my-car = ...
  my-cdr = ...
  my-set-car! = ...
  my-set-cdr! = ...

|#

(define x (my-cons 1 2))

#|

G: (parent = nil)
  ...
  x = ..., env = E1

E1: (parent = G)
  x = 1
  y = 2
  set-x! = ...
  set-y! = ...
  dispatch = ...

|#

(define z (my-cons x x))

#|

G1: (parent = null)
  ...
  z = ..., env = E2

E1: (parent = G)
  ...

E2: (parent = G)
  x = ... (from x in G)
  y = ... (from x in G)
  set-x! = ...
  set-y! = ...
  dispatch = ...

|#

(my-set-car! (my-cdr z) 17)

#|
; evaluate (my-cdr z) first

G: ...

E1: ...

E2: ...

E3: (parent = G)
  z = ... (from z in G)

E4: (parent = E3)
  m = 'cdr

; then my-set-car!
E5: (parent = G)

|#

(out (my-car x))

(end-script)
