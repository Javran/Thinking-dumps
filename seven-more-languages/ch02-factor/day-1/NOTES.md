## Applicative vs. Concatenative

* *applicative language*: In most of the programming languages you apply a function
to a value. These languages are called *applicative*.
* *concatenative language*: In languages like Factor, you simply write functions one
after another (concatenate them), the net effect is that all functions are applied in
the written order.

## Listener

The usual way of playing with Factor is to use the Listener shipped with it.
As commands are inputted one after another, data stack is visualized.
Note that the stack grows "downwards", meaning the top of the stack is
the last one printed in the listener.

## Values as Booleans

Just like Lisp family, everything other than `f` is considered true.

## Sequences and Quotations

Use `{ 1 2 3 }` to create and push the sequence onto the stack. `{` is a command
that begins sequence creation and `}` terminiates element listing, so be aware that
proper spaces should be inserted.

Quotations begin with `[` and end with `]`, when an expression is quoted, it will not be executed.
Use `call` to fetch the quoted expression on top of the stack and execute it.

## Stack Shuffling

To apply a function, we need to make sure the values are pushed onto the stack in right order,
which could be a pain from time to time. There are stack shuffling operations available to
duplicate, drop or swap the order of items on the stack. Futher there are higher-order
combinators we can use instead of doing them manually.
(There are many of those, not going to list them all)
