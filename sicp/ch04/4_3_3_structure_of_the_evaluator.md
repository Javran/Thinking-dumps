Changes:

* new special form `amb`,
`amb-choices` fetches all possible values from the expression

* way to dispatch the handler for this new special form,
I'd like to reuse `my-eval`, but it's also possible to
use the underlying `mit-scheme` to relieve some pain.
(To support both `analyze` and `interpret` mode might be an inproper design,
since some features cannot be easily implemented correctly or efficiently
in `analyze` or `interpret` mode, while the existing system requires
the correct implementation of both.)

* `ambeval` is a procedure that accepts 4 arguments:

    * `exp`: the expression
    * `env`: the environment
    * `suceed`: callback when the compuation has succeeded
    * `fail`: callback when the compuation has failed

    (P.S.: I really think `succeed` and `fail` are ugly hacks,
    which makes the control flow implicit and readability poor.)

* complexity of the system comes from the mechanics of passing the
continuations around.
