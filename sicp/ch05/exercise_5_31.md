In the following table, the meaning of last 3 columns are:

* `env`: the necessity of saving and restoring `env` register
  **around the evaluation of the operator**
* `argl`: the necessity of saving and restoring `argl` register
  **around the evaluation of each operand**
* `proc`: the necessity of saving and restoring `proc` register
  **around the evaluation of the operand sequence**

Expression | `env` | `argl` | `proc`
--- | --- | --- | ----
`(f 'x 'y)`     | superfluous | superfluous
`((f) 'x 'y)`   | superfluous | superfluous
`(f (g 'x) y)`  | superfluous | necessary
`(f (g 'x) 'y)` | superfluous | necessary

The basic rule is that if the register does not change when
evaluating a certain expression, then it is not necessary to
save and restore that register in stack.

* For `env` register, if the operator is just a symbol, then
it is not necessary to save and restore it. Because
variable lookup will not change the value of `env` register.

    However, if the operator expression is itself a function
    application, then it is necessary to keep `env` on stack,
    because evaluating a function application might require
    the `env` register to change to bind some new variables
    in order to evaluate the function body.

    In the given 4 expresions, only `((f) 'x 'y)` will potentially
    change the value of `env`, but its arguments are all quoted symbols.
    Therefore it is not necessary to save and restore `env` anyway.

    A situation where keeping `env` when evaluating the operator is
    necessary would be evaluating the expression `((f) x y)`, in
    which case both of its arguments requires variable lookup
    on the `env` register which might be changed during the
    evaluation of `(f)`.

* When evaluating operands, `argl` will be changed if the operand
itself is a function application. So to determine if it is necessary
to keep it on stack, one just need to examine if its operands have
some function applications. So for the last two expressions,
saving and restoring `argl` register is necessary while
it is not necessary for the first two expressions to save and restore them.
