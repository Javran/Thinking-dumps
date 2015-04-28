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

To proceed, we can remove `a` from the list. Note that labels are just indicators
and perform no operation themselves, we can safely remove them when we have done
dealing with them.

For our example, we can remove `a` from the left list:

    b          (a b)
    (a b)      (c d e)
    d          (e)
    (c d e)    (f)
    (e)        (h)
    (f)        (i j k)
    g
    (h)
    (i j k)
    l

And then we can do the same thing with `b`:

    (a b)      (a b)
    d          (c d e)
    (c d e)    (e)
    (e)        (f)
    (f)        (h)
    g          (i j k)
    (h)
    (i j k)
    l

Now the head of list on the left is not a label,
which should exactly be the head of list on the right.
Now we are dealing with no labels, and we can ignore them
by moving our attention to the tails of both.

    d          (c d e)
    (c d e)    (e)
    (e)        (f)
    (f)        (h)
    g          (i j k)
    (h)
    (i j k)
    l

Then the binding for `d` is found, and we remove it.

    (c d e)    (c d e)
    (e)        (e)
    (f)        (f)
    g          (h)
    (h)        (i j k)
    (i j k)
    l

As before, we can skip as many instructions
as possible if they are not labels and matches exactly:

    g          (h)
    (h)        (i j k)
    (i j k)
    l

Same as before, after we have found the binding for `g`,
we can proceed by removing matching instructions.

    l          <empty>

At this point there is nothing left on the right side,
but it is totally fine for `l` to bind to an empty list.

Note that this whole process goes through both of the instruction
lists exactly once, and creates no extra space for storing
the list of instructions.
