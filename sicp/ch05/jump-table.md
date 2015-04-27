# Some documents about how `build-jump-table` works

Say we have the following list of instructions:

    a
    b
    (a b)
    d
    (c d e)
    (e)
    (f)
    g
    (h)
    (i j k)
    l

The first step is to have the same list of instructions
but with all labels removed (just reducing some overhead
of interpreting labels at runtime):

    (a b)
    (c d e)
    (e)
    (f)
    (h)
    (i j k)

If we build the jump table naively, every labels
need to have a list of instructions.
But the list of instructions have many overlaps
and certainly can be shared between labels.
To save more spaces, we build the jump table by merely
reusing the no-label version of the instruction list.

We align two lists to make the initial state:

    a          (a b)
    b          (c d e)
    (a b)      (e)
    d          (f)
    (c d e)    (h)
    (e)        (i j k)
    (f)
    g
    (h)
    (i j k)
    l

Note that the list `(a b) (c d e) ...` is already the corresponding
list that `a` points to. It turns out we can take advantage of maintaining
this invariant to build up the jump table.

