If change `f`'s formal parameter to `a` the code becomes:

    let a = 3
      in let p = proc (z) a
         in let f = proc (a) (p 0)
            in let a = 5
               in (f 2)

build up environment:

    Env:
    <init env>
    a = 3
    p = proc (z) a     ; free-vars = a
    f = proc (a) (p 0) ; free-vars = p
    a = 5

    Exp:
    (f 2)

Evaluate `(f 2)`, bind only free variables
with their value evaluated in the current environment:

    Env:
    <init env>
    p = proc (z) a
    a = 2

    Exp:
    (p 0)

Evaluate `(p 0)`.

    Env:
    <init env>
    a = 2
    z = 0

    Exp:
    a

The result is `2`.

See also the last two testcases in
either `./ex-3.28/ds-rep/tests.rkt`
  or `./ex-3.28/proc-rep/tests.rkt`.
