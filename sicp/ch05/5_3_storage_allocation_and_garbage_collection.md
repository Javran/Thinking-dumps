# Storage Allocation and Garbage Collection

Two considerations about implementing list structure:

* Representation

    * We need "boxes" and "pointers"
    * Typical computer memories have storage and addressing capabilities available
      we will try to use only these two capabilities.

* Memory Management

    * Need to continually create new data object
    * Real computer memories are finite
    * Automatic storage allocation / garbage collection

        * When a data object is no longer needed,
        its memory address is reclaimed for future data objects
        * Keep the illusion of an infinite memory

## Memory as Vectors

Vectors can be read from or write to, and the access location
is independent of the index (i.e. support efficient random access).
Computer memories can be mimicked by vectors.

Vector has the following two primitive procedures to support
reading and writing.

* `(vector-ref <vector> <n>)` returns the n-th element
* `(vector-set! <vector> <n> <value>)` sets the n-th element to `<value>`
