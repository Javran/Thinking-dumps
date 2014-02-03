Louis's `map` fails because
The underlying scheme is using a different
representation of procedures and the environment.

The primitve `map` is expecting a primitive procedure
on its first argument, and the underlying scheme knows
nothing about the procedure representation used by the
implemented language.

Eva's `map` works simply because this `map` is implemented
within this implemented language.
