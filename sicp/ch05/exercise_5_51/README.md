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

## Notes


* A principle of allocationg and deallocating objects

    A procedure who is responsible for deallocating objects,
    should only deallocate object contents.
    And the caller to this procedure should be responsible for
    deallocating the pointer to that object.

    (The mistake I made before is to pass a deallocating-function
    object-pointer pointers so that the function can handle the object
    pointer deallocation and set the object pointer to 0.
    This would complicate the process of deallocation.
    More importantly, the object pointer is just a value that can be
    assigned to another variable, therefore it does not make much sense
    to let the deallocating-function aware of the object-pointer pointer)
