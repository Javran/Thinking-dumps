Plan

* a standalone version of metacircular evaluator (require as few primitive
defined functions as possible)

* need some tests

* think more careful about the compiled evaluator.
for now `apply` doesn't work and we are relying on a hack
to solve the problem. With two layers of wrapping,
we need to make the intermediate level able to "unwrap"
the primitive and pass it to the real "apply" function.
