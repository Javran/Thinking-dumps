Recall what is normal order and what
is applicative order:

* Applicative order: all the arguments are evaluated
when the procedure is applied. (call-by-value)

* Normal order: evaluation of arguments are delayed
until the actual argument are needed. (call-by-name)

Strictness:

* a procedure is entered before an argument has
been evaluated, then the procedure is "non-strict" in that argument.

* in applicative languages, all procedures are strict
in each argument.

* in normal languages, all compound procedures are non-strict
in each argument, but primitives might be strict / non-strict
in some arguments.
