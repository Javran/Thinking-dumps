The approach described by the exercise has a tiny drawback that
we cannot set breakpoint if there is no label before the intended instruction.
But this is easy to fix since
we can always add a dummy label right before the first instruction.

I have implemented the breakpoint point by following steps:

* Maintain the last seen label name during execution

    As this provides us a clue about when should we stop
    if there are some breakpoint settings.
    Because the execution can only jump to positions who have
    labels, the "last seen label" should always be the current label
    in which we want to insert breaks.

* A structure to keep all informations about breakpoints

    I use an assoc-list, whose keys are those labels,
    and values is a bag of integers. These integers stands for
    the offset where we want to put our breakpoints in.

    Before every instruction gets executed, we check this structure
    to see if we should let it go or stop right here to implement
    the breakpoint feature.

    It is true that we could have better performance by caching
    the bag of integers that corresponds to the current label,
    but for simplicity we choose not to do so.

* A resuming flag

    Since breaking a program is just not to execute next `execute!` procedure,
    we can use `execute` procedure itself to proceed.
    But this might not work as expected because there is no distinction
    between about to break the program and about to resume the program.
    So simply run `execute` will end up with stopping at that breakpoint,
    which is not intended. We address this problem by introducing a new flag,
    I call it "resuming-flag", this flag is initialized to false and will remain
    being false until a resume requests is fired. In which case it will be set to
    true. When the program meets a breakpoint, it will also check
    this resuming-flag, if this is set to true, then set it to false
    and ignore the breakpoint, otherwise we are not resuming, and
    the machine will know that it should stop there.
