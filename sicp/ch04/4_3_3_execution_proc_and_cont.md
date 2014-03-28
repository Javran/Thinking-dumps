Three arguments to `amb` evluator:

* the environment
* continuation procedures
    - success continuation:

        the evaluation results in a value,
        receive the value and
        proceed with the computation.

    - failure continuation:

        the evaluation results in a dead end,
        should cancel the side effects and
        try another branch.

My note:
Failure continuations are called whenever we need to try alternatives
(either by user or by `require`), if no more alternatives can be tried,
the outer failure continuation is triggered to achieve the backtrack process.

This implementation works only for one thread, because you can only try
all alternatives in a predictable order, and when you are trying one choice,
it is unsafe to attempt other alternatives otherwise the cancelling of side effects
might not work properly.
