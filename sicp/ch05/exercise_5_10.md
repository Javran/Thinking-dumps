The requirement for this exercise is just "design a new syntax"
It is not necessary to make everything new, which is boring.
Instead, I will just change the syntax of an existing instruction
and call it a new design.

Syntax change:

* remove `assign` and split its functionality
into two new instructions: `copy` and `call`

* `(copy <reg> <src>)` copies value from `<src>` to `<reg>`.
`<src>` can be one of: `(reg <reg>)`, `(const <const>)` and
`(label <label>)`.

* `(call <reg> <op> <arg1> <arg2> ...)` calls the primitive `<op>`
using `<arg1> <arg2> ...` as arguments, and assign the result
to `<reg>`. Arguments can be one of: `(reg <reg>)`, `(const <const>)` and
`(label <label>)`.
