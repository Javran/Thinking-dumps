# Directory `./data-like-proc/`

This is essentially a data-structure-like
procedural representation.
Where each field is replaced with a procedure
which will return the value of that field when called.

It might not be the suggested way of implementing
the procedural representation.
But I choose to keep it, because this is much like
the way `define-datatype` (not sure about
what is the underlying implementation
but at least it appears to be) does it.

# Directory `./real-proc/`

Continuations now are really just procedures.
With all the value needed saved in the environment
which is carried with that procedure,
less lines of code can be achieved.
