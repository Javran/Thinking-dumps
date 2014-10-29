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

# Implementation of a stop-and-copy garbage collector

TODO

I think there are still many details missing,
maybe I want to have a concrete example and see it in action.

TODO

It seems like we have to go back and re-examine the list primitives.

Need a template framework, we will replace instructions by recognizing them,
extracting register names and replace one instruction at a time by a list of instruction which
implements it.

Also it seems like there's no distinction between numbers and pointers if we just look at
the memory. I guess here we distinguish them by their usages. Let's implement it and see
if my observation is correct.
