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

* `Parser`

    Language parser that takes token and converts them into S-exps

* `SExp`

    Implements S-Expression. `newXXX` family allocates a new object,
    and the user is responsible for releasing the resource.
    and `freeSExp` non-recursively frees that object.

* `Environment`

    Implements runtime environment object that can be nested, inserted
    and lookup-ed.

* `Frame`

    One single frame from an environment, which stores key-value bindings

* `InitEnv`

    Initial environment that provides only primitive values

* `FunctionObject`

    Represents compound procedures and primitive procedures


* `Evaluate` and `EvalXXXX`


    `Evaluate` contains the definition of `evalDispatch`,
    which is used for looking up proper handler and dispatch
    to it. `EvalXXXX`s are for the actual implementation of
    each handler.

* `PointerManager`

    For keeping track of runtime-generated objects.
    After initializing the PointerManager, any resource
    can be registered with a custom de-allocation handler.
    And the resource will be released properly when the PointerManager
    is released.

* `ManagedSExp`

    Managed SExp are allocated at runtime and automatically registered
    in the PointerManager.

* `Primitives`

    The implementation of primitive functions for `InitEnv`

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
