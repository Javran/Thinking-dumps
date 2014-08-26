The original program is:

    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))

To design a register machine, we must design its data paths and the controllers.
Data paths are registers and operations and the controllers are sequence of these
operations.

Note: Maybe here I can make an analogy between a register machine
and a register based bytecode machine. Data paths are instructions including
the way to pass values among registers and the way to combine certain values
to yield a result. And the controller might be like a sequence of instructions
to arrange these instructions to form a meaningful program.

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

About controller diagrams:

* Rectangles: data path (the "button" to be pushed)
* Arrow: sequence
* Diamond: a decision
