In GCD machine, the final result is just the result of solving its sub-problem.
However, in problem `(factorial n)`, we need to first compute `(factorial (- n 1))`,
which needs to be multipled by `n` to get the final result.

The current approach requires the final answer being exactly the answer to the
subproblem, or otherwise some registers and states might be overridden rendering
it difficult to resume from the previous recursive call.
Therefore we need to be able to save more states before calling
the recursive solver for subproblems.

New instructions: `save` and `restore`. (Represents a stack,
we need a "last in, first out" data structure because
in a nest of recursions the last subproblem to be entered is the first
to be finished.)
