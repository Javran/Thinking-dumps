As long as it's guaranteed that `withdraw` and `deposit` will call `set!` exactly once,
it's not necessary to put `balance` into protection.
Because `withdraw` and `deposit` is already serialized,
the value of `balance` can only be a final result of one operation
(if `balance` is mutated only once during each operation).

However, it's still possible that `balance` get modified right after `balance` is called.
So we don't trust the return of `balance`, instead, as what we've done in `deposit` and `withdraw`,
we access the variable `balance` directly inside a serialized operation.
