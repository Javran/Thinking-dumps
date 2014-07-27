I'm afraid that this "optimization" won't work
in general. Therefore I'm done with this exercise and
here I'll explain why.

The problem lies in the conjunction itself. Although it sounds good to
break conjunction into sub-queries, but all these queries are related to each other.

Let's revisit exercise 4.64, in which we are told the order of the query matters:
the first few queries might provide important hints to narrow down the search space,
By breaking `and` expressions into parts, we might lose the benefit of narrowing down
search spaces and might even cause infinite loop with even correct queries.

So here I argue that this "optimization":

* Might not guarantee correctness because even for a correct query
the optimized program might end up in an infinite loop (which can be considered an error
and therefore an incorrect answer)

* Might not have less frames to work with, because by breaking conjunctions,
we might not have sufficient frame bindings to narrow down the search space.
