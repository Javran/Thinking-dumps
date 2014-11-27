# Maintaining the Illusion of Infinite Memory

The basic idea is that many intermediate results will never be used
again and become garbage, and we can reclaim the space taken by them.
If we can recycle memory at about the same rate at which we construct
new pairs, we can preserve the illusion of infinite memory.

To tell if a pair is no longer needed, we check if the location in question
can be accesssed by a sequence of `car` and `cdr` from any of the current
accessible registers. If the location isn't accessible at all, then
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

* Note that rewriting the original instruction list
might make tracing difficult,
because we needs to keep track of the correspondence
between original instruction list and the new one.
This is doable by bookkeeping the relationship when apply rewriting rules,
but I think this might be complicated and I'm not going to try that.

# Implementation of a stop-and-copy garbage collector

For me I think the explanation for garbage collecting code
in book isn't very clear about how it really works.
Here I'll try to provide an alternative explanation for myself
and hope this can be helpful for those that happens to find this article.

The most important registers in code are `free`, `scan`:

* unless we are creating pairs, `free` always points to the next unused memory location.
* `scan` points to the location where we are about to "update" the data under pointer.

These two registers together define 3 consecutive regions in the new memories:

* the region between the first valid address of the new memory and `scan - 1`
is the region that our garbage collecting algorithm has fully processed (a deep copy).
* the region between `scan` and `free - 1` is the region that data is simply copied from
the old memories (a shallow copy). This means if old data contains pointers,
these pointers are still pointing to somewhere in the old memory address.
* all addresses after `free - 1` contains no data and has nothing to do with the algorithm.

For [shallow copy](http://en.wikipedia.org/wiki/Object_copy#Shallow_copy)
and [deep copy](http://en.wikipedia.org/wiki/Object_copy#Deep_copy),
you can find more explanation on wikipedia. Here you can replace "object" with "pair"
so it makes more sense.

The algorithm assumes a register `root` that can eventually access every reachable data
by using a sequence of `car`s and `cdr`s. Therefore we ensure that all accessible
data are copied from the old memories by having a deep copy of `root`.
Note here the structure of `root` doesn't matter as long as it can point to all accessible
data directly or indirectly. For simplicity we will make it a list, but it should also
work it you choose to make it look like a tree or something else.

The high level idea here is that we will first create shallow copy for the object that
`root` points to, then we turn the shallow copy into a deep one.
By doing so, we will create more shallow copies. We repeat the process of dealing with
shallow copies and dealing with deep copies
until all shallow copies are turned into deep copies.

The algorithm begins by dealing with `root`: if `root` is a non-pair data,
all accessible data should have been copied so the algorithm terminates here.

Otherwise if `root` is a pair, this pair are shallow-copied into the new memories,
and `free` memories will increase accordingly.

Now that the region between `scan` and `free` is no longer empty,
and `scan` is pointing to the location where the first shallow copy is available.
We proceed by turning the data under `scan` into a deep copy and make `scan` point to
the next shallow copy by adding one to it.

To turn a shallow copy into a deep one, one need to access the old memories
and copy the pair indicated by this shallow copy into the new memory.
Once this is done, the data in old memories can be safely discarded.
We turn the original data in old memories into a `broken-heart` flag
by setting a constant `broken-heart` in its `car` part, and the new location
in new memories in its `cdr` part. When another shallow copy wants to copy the
same pair, the algorithm will find out that the data has been copied and
find the location in `cdr`.

Now that we will have some new shallow copies, and `scan` won't be able to
catch up with `free` unless all shallow copies have turned into deep copies.
Just repeat the process and the algorithm will eventually terminate at some point.

There are two registers `new` and `old` which are mainly used by `relocate-old-result-in-new`
and `gc-loop`.
`old` is the input register,
which is either a non-pair value or a pointer pointing to somewhere in the old memories.
and `new` is the output register,
which is either a non-pair value or a pointer pointing to somewhere in the new memories.

Calling subroutine `relocate-old-result-in-new`
results in copying data in `old` into `new`. And data pointed by `new` should be a deep copy.

Subroutine `gc-loop` handles the `scan` register, it traverses every `car` and `cdr` of every
memory location and turn data into its deep copy.

And one thing is important: you need to pre-allocate some memory addresses to ensure that every
not-gc-implementation-related register has its place to store its value.
And also we should have a pointer that can directly or indirectly points to all these
pre-allocated memory addresses. This pointer becomes `root` in the gc algorithm.

Right before the garbage collecting starts, we need to dump values from all the registers to
this list. And after garbage collection is done, we need to recover register values
accordingly since old pointers are no longer valid.

Now I have implemented the garbage collector, here are few things that I think
is important but not mentioned in the book:

* How to save and recover normal registers

    There are two kinds of registers: one is related to the garbage collecting algorithm
    and the other kind of registers don't have the concept of garbage collection in mind.
    All gc-related registers are not required to be stored somewhere else, as they serve
    the mere purpose of doing the garbage collecting job and should never be used for other
    purposes. And also when we are writing the machine code, we won't touch these registers
    at all. In my garbage collecting algorithm, I try to avoid the confusion between garbage
    collecting related register and normal registers by prefixing the former ones with "gc-".
    However, some important registers like `root`, `the-cars`, `the-cdrs`, `new-cars`, `new-cdrs`
    and `free` are still being kept as it is.

* How to make the decision of performing garbage collection

    Observe that the only thing that increases the `free` is the instruction:

        (assign free (op ptr-inc) (reg free)))

    So we can insert some instructions after every occurrence of this instruction.
    And this should result in checking the current value of `free` against memory size,
    and perform garbage collection or do nothing, and eventually resume the computation
    after making the decision and doing the right operation.

* How to jump to the decision-making subroutine and jump back

* Generate instructions for `root` reallocation and register saving and restoring
