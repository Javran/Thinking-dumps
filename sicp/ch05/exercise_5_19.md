No idea for now. Maybe some techniqbue from exercise 5.17 might be helpful.
The approach described by the exercise has a tiny drawback that
we cannot set breakpoint if there is no label before the intended instruction.
But this is easy to fix and we can always add a dummy label right before the first instruction.

An early plan:

* keep a metadata of `label <-> a list of breakpoint numbers`
* maintain the last seen label name during execution
* a instruction counter which will be reset every time
  a new label is reached
* break the execution loop if there is a match in metadata
* to set / unset breakpoint, one simply change the metadata
