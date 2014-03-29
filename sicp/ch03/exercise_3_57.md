When `fibs` are defined based on `add-streams`, `fibs[n-1]` and `fibs[n-2]`
are already calculated. Therefore, for `n >= 2`, `n-2` additions are performed.

When we do not use memoization, we need to calculate `fibs[n-1]` and `fibs[n-2]`
all over again before we can calculate `fibs[n]`.

Let `G(n)` be the number of additions needed to calculate `fibs[n]`

* `G(1) = 0`
* `G(2) = 0`
* `G(n) = G(n-1)+G(n-2)+1`
* `G(n) + 1 = (G(n-1) + 1) + (G(n-2) + 1)`

So `G(n) + 1 = fibs[n]`, which shows that `G(n)` is exponentially greater.
