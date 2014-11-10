# Maintaining the Illusion of Infinite Memory

The basic idea is that many intermediate results will never be used
again and become garbage, and we can reclaim the space taken by them.
If we can recycle memory at about the same rate at which we construct
new pairs, we can preserve the illusion of infinite memory.

To tell if a pair is no longer needed, we check if the location in question
can be accesssed by a sequence of `car` and `cdr` from any of the current
accessible registers. If the location isn't accessable at all, then
we can reclaim its space because it won't effect the future of the computation.

One garbage collection method: *stop-and-copy*.
Have two memories (i.e. working memory and free memory),
we always allocate new spaces and work with working memory.
When it is necessary, we locate all useful data and move them into the
free memory, and then the role of free memory and working memory are switched
so after the garbage collection we are actually working on the memory which
previously was free memory. The old working memory becomes free memory and
its cells are either not useful or empty, so its entire space can be reused.

Here are some of my notes (based on my real implementation of this system):

* If we need to implement primitive predicates like `pair?` and `number?`,
then some tags on data are necessary, without which we can not tell if an integer
number in memory is in fact representing an integer or representing a memory address.

* Since `free` register is indicating a memory location, it would have some extra
tags to make it different from integer numbers, so the normal `+` operation might
not work for `free` register unless some modifications are done.

    Meaning that the following instruction might not work as expected:

        (assign free (op +) (reg free) (const 1))

    In my implementation, the instruction above is changed into the following one:

        (assign free (op ptr-inc) (reg free))

    Here a memory location is tagged with some extra data to make it different from
    normal integers. And `ptr-inc` works by adding one to that memory location while
    preserving that extra tag.

* To save some boring and error-prone task of translating instruction into
some more lower-level operations, we might need a template system that finds
the place where a modification is needed, and apply a template automatically.
For example, the template system might recognize the following instruction:

        (assign p (op cons) (reg r1) (reg r2))

    By comparing it with a pattern:

        (assign <reg1> (op cons) (reg <reg2>) (reg <reg3>))

    finding the bindings:

        reg1 = p
        reg2 = r1
        reg3 = r2

    And eventually rewriting it into something like:

        (perform (op vector-set!) (reg the-cars) (reg free) (reg r1))
        (perform (op vector-set!) (reg the-cdrs) (reg free) (reg r2))
        (assign p (reg free))
        (assign free (op +) (reg free) (const 1))

    And this functionality is implemented in `rewrite.scm`.
    Amazingly, if you look at the implementation, you would find out
    that it is strongly resembling the logic programming system that we've implemented before,
    indeed this is essentially doing pattern matching, variable capturing and replacing.

* There are some cases where more general pattern can be used instead of those
found in the book. For example, the rule of replacing `set-car!`:

        (perform (op set-car!) (reg <reg1>) (reg <reg2>))

    With the following instruction:

        (perform (op vector-set!) (reg the-cars) (reg <reg1>) (reg <reg2>))

    Here we find that in pattern `(set-car! <a> <b>)`, `<b>` can be something other than
    a register: a constant will also do.
    So in `list-stack-rewrites.scm` I've refined some of these patterns to make it
    more general.

TODO: make it work with the legacy one?

# Implementation of a stop-and-copy garbage collector
