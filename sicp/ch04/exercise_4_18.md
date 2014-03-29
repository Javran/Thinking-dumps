Let's try to do the expansion:

    (define (solve f y0 dt)
      (define  y (integral (delay dy) y0 dt))
      (define dy (stream-map f y))
      y)

    => <2-layer-trans>
    (define (solve f y0 dt)
      (let (( y '*unassigned*)
            (dy '*unassigned*))
        (let ((a (integral (delay dy) y0 dt))
              (b (stream-map f y)))
          (set!  y a)
          (set! dy b))
        y))
   
or, alternatively:
    => <1-layer-trans>
    (define (solve f y0 dt)
      (let (( y '*unassigned*)
            (dy '*unassigned*))
        (set!  y (integral (delay dy) y0 dt))
        (set! dy (stream-map f y))
        y))

The first transformation is not working,
in which the `b` is bound to `(stream-map f y)`
but the value of `y` is still `'*unassigned*`.
