* Approach #1: make the input register the same. For example, whenever we use a GCD compuation,
we always use registers `a` and `b` as inputs instead of `c` and `d`
or something else if the compuation are required to be performed for multiple times.

    Problem: If we want to perform compuations which only differs in input, then in the
    current approach we have to duplicate the same components for multiple times,
    which sounds not good.

* Approach #2: set a special register `continue`, before jumping to the sub-compuation,
  resume by analyzing the value of `continue`.

    Problem: for each sub-compuation usage, we need to write a corresponding branch
    to handle the resuming.

* Approach #3: save the current label in the `continue` register, when resuming,
  fetch this value and jump to the position indicated by `continue`.

    Problem: if a subroutine calls another subroutine, we need to make sure
    that the callee subroutine keeps `continue` somewhere, or otherwise
    it might not be possible for the callee subroutine to resume (because
    the callee subroutine has to override `continue` in order to correctly
    resume from another subroutine.)

New functionality:

* `(assign <reg-name> (label <label-name>))` to assign a label to the register
* `(goto (reg <reg-name>))` to jump to a label
