# If `(halts? p p)` returns true when `p = try`

Then `(try try)` should halt normally.
However, since `(halts? p p)` returns true,
the consequent get executed, which ends up in an infinite loop.

# If `(halts? p p)` returns false when `p = try`

Then `(try try)` should never terminate.
However, `(try try)` will terminate and return `'halted`.
