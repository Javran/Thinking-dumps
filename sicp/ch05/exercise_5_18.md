There are more than one way to implement
register tracing. One of them is to have the
target register keep a tracing flag.
And another way is to have a "hook"
in the register value setter.

The exercise is suggesting modifying
`make-register` to implement this functionality.
So for the legacy simulator I'm going to follow
what this exercise has suggested.

And for the `simu.scm`, I will try to take
this second approach.

Keep in mind that I still prefer the approach
suggested by the exercise. Because the first approach
organizes register-related values all in one place,
which is not only convenient but also efficient because
the only lookup you need is the register lookup.

However for the second approach, if the register content
setters are not the only procedure that changes the content
of a register, then we might be in trouble. In addition,
storing the register-tracing flag elsewhere means we might
need to lookup the same register twice: one to find the register
and read from or write to it, and another to find the register
tracing flag in some extra machine data fields.
But I think if we only need to trace a limited number of registers
(and I think this is the usually case when you are debugging something)
this approach won't hurt much.
