## Files

* `DynArr`

    A simple array implementation that supports only insertion,
    allocates extra space when running out of memory

* `Syntax`

    Syntax-related functions used by the tokenizer.
    Simplified from R5RS to deal with only necessary
    syntax.

* `Token`

    Implements token structure. `mkTokenXXX` family accepts
    a token pointer and writes data to it, while `freeToken`
    takes a token pointer and frees the inner content.

* `Tokenizer`

    Traverses a string and tokenizes it into a dynamic array.

* `SExp`

    Implements S-Exprssion. `newXXX` family allocates a new object
    and `freeSExp` frees that object.

* `Common`

    Contains some commonly shared stuff.

* `Util`

    Utilities.
