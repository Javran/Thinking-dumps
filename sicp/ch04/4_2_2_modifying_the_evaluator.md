I choose not to do the modifications to `my-eval`,
so for the next few exercises, I will just guess what will
happen rather than implementing this feature and observe
the result. If necessary, I'll do it using native
scheme programming and make use of `delay` and `force`
as what we've done in chapter 3.

Summary of the modifications:

* `actual-value` looks like `force` wrapped around `eval`, which
forces the value and produce a non-thunk result.
* for function application, only force the operator,
but delay the evaluation of operands.
* for compound procedures, the arguments are keep delayed.
* for primitives, we force all the operands.
* the implementation of `if` needs to be changed,
because that special form `if` is "strict" in the predicate part.
