* What order of evaluation does our compiler produce
for operands of a combination?

    The operands are evaluated from the rightmost one to the leftmost one.

* Where in the compiler is this order determined?

    `construct-arglist` reverses the argument list so that
    the operands are evaluated from the last one to the first one.

* Modify the compiler so that it produces some other order of evaluation.

    See examples in `./exercise_5_36.scm`.

* How does changing the order of operand evaluation
affect the efficiency of the code that constructs
the argument list?

    The difference lies in the efficiency between inserting
    the evaluated value in front of the original list
    and appending the value to the original list.

    `cons` has a better performance because it does not require
    to traverse the original list while appending elements to a list
    might require one to traverse the list and modify the original data structure.

    However, `cons` inserts elements backwards, which means to evaluate a sequence of
    expressions `a;b;c;d`, it will accumulate the evaluated value in order `d;c;v;a`.
    Although not as efficient as `cons`, if we insert elements to the end of the original list,
    the order will be preserved.

    Less efficiency penalty can be achieved by using more efficient data structures
    like vector: since we know how long the argument list will be, we can construct
    it beforehands. And accessing the vector is efficient and does not require traversal.

    Changing the order of operand evaluation has nothing to do with the efficiency of the code,
    because we can use more efficient data structures like vector which does not care the order of
    constructing the argument list. But if we are talking about the specific implementation that
    uses lists, evaluating the code from right to left is slightly more efficient.
    However, as we have seen from the code, this complicates the source code a lot and I think
    the right-to-left operand evaluation does not justify the performance gain.
