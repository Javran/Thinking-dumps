There are actually two approaches to this exercise.
I'll try to cover these two approaches
in my implementaion.

I will only use `simu.scm` to simulate the machine.
Since the selection of simulator won't make much difference
in result.

# Approach 1

It would be much better to rerun a machine
with different register values set up.

For the current model, initial value of registers
are given when the machine is built,
therefore not suitable of doing this,
So first thing to make sure is that
we can build the machine once for all,
with registers being able to be initialized
multiple times.

# Approach 2

I believe this approach is what the exercise
expects us to do: manipulate the list of instruction
so that we can have a outer loop to iterate through `n`.
And `print-statistic` can be called using instructions.

Here the disadvantage would be that this approach might potential
have some weird problems because registers are not properly
initialized after the first loop. One need to be extremely careful
not to leave any thing uninitialized from last execution or
otherwise a potential error will cause bugs.

Another problem might lie in `initialize-stack` operation,
as it might potentially destroy many states stored in
the stack making the machine into a confusing state.

# Result

| n | number-pushes | max-depth
| --- | --- | --- |
| 2 | 2 | 2 |
| 3 | 4 | 4 |
| 4 | 6 | 6 |
| n | 2n-2 | 2n-2 |

From the result we know that
the total number of pish operations and
the maximum stack depth are both `2n-2`.
