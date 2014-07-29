The original program is:

    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))


About data path:

* Rectangles: registers

* Arrow with "X" behind the head: way to assign values

* Trapzoid: operation that computes a value from constants and the contents
of the registers

* Circles: tests (no output arrow, used by controller)

According to the data path, we have the following possible
operations:

* `a <- b`
* `t <- r` `r <- rem a b`
* `b <- t`
