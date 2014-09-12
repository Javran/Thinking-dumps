There are more than one way to implement
this exercise.

One is to split the stack into individual
stacks for each register, which is described
in detail in the exercise.

Another is to drop the global stack
entirely, and the register values
will now be stacks.
The register value is now the top element
on that corresponding stack, and
"save" and "restore" will operate on
the stack specified by the register.

The first strategy will be implemented
in simulator "legacy-easy.scm"
while the second one "simu.scm".
