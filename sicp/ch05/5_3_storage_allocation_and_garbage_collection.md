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

## Representing Lisp data

### A Possible Representation for List Structure

The book proposed a way of representing list if the memory can be divided
into two vectors: `the-cars` and `the-cdrs`.

An example:

    ((1 2) 3 4)

Equivalent form:

    ((1 . (2 . ())) . (3 . (4 . ())))

Since `(<x> . <y>)` is just `(cons <x> <y>)`, each dot will take
one memory location. So as expected, 5 memory locations are used.
(There are two vectors, and we are assuming these two vectors are
of the same size, a memory location can point to two actual locations
by using memory location combining with `car` or `cdr`. The memory location
tells us which pair we are looking for, and `car` and `cdr` tell us
which part of that very pair we are looking for.)

Now we make some notations: `n<num>` is used for numbers,
`p<location>` is used for locations, `e0` is used to represent
empty lists.

Now let's consider every pair for inside to outside.
And here the location number assignments agree with one shown in
figure 5.14.

Consider pair `(2 . ())`, the first part will be stored as `n2`
and the second part `e0`. We store it in location `7`:

`Location` | `the-cars` | `the-cdrs`
--- | --- | ---
`7` | `n2` | `e0`

Then similarly for pair `(4 . ())`

`Location` | `the-cars` | `the-cdrs`
--- | --- | ---
`7` | `n2` | `e0`
`4` | `n4` | `e0`

Now for pair `(1 . (2 . ()))`, we want second part of this pair
point to the location of that `(2 . ())` pair, which is `7`, therefore:

`Location` | `the-cars` | `the-cdrs`
--- | --- | ---
`7` | `n2` | `e0`
`4` | `n4` | `e0`
`5` | `n1` | `p7`

Similarly for pair `(3 . (4 . ()))`:

`Location` | `the-cars` | `the-cdrs`
--- | --- | ---
`7` | `n2` | `e0`
`4` | `n4` | `e0`
`5` | `n1` | `p7`
`2` | `n3` | `p4`

Finally, to represent the whole list,
we put together `(1 . (2 . ()))` and `(3 . (4 . ()))`:

`Location` | `the-cars` | `the-cdrs`
--- | --- | ---
`7` | `n2` | `e0`
`4` | `n4` | `e0`
`5` | `n1` | `p7`
`2` | `n3` | `p4`
`1` | `p5` | `p2`

And this is exactly the table represented in figure 5.14.

### Tag data

We add some tag to the data so that the system can
have an idea about how these data should be used.
Previously in Chapter 2, we just tag the data by making pairs.
But here, we extend the notion of pointer to make it typed.
One way to achieve this might be allocating some extra space in addition to
one that the data really needs, and some bits might be put in this extra space
to indicate what kind of data is this. Just as previously when we were
representing a list structure, we use `n` `p` `e` as prefixes
so that we don't get confused when seeing the number following that tag.

### `eq?` and symbols

Refering to the book, `(eq? <x> <y>)` returns a true value only
if `<x>` and `<y>` have the same pointer. And since we know that for symbols
they are only equal if their string representations are equal,
the allocator must guarantee that whenever two symbols with the same
string repsentation are created, they are pointing to the same location.
This is usually achieved by keeping the string representation - assigned pointer
relation somewhere. This table is called **obarray**.
And **interning** symbols refers to the process of
replacing strings by unique pointers.

### Implementing the primitive list operations

See book for the actual implementation.

This implementation assumes the availability of:

* `vector-ref`
* `vector-set`
* `the-cars`
* `the-cdrs`
* `free` (a pointer that always points to a free space)

### Implementing stacks

Stacks are just lists.
For efficient stack implementation,
one usually uses separated vector
to represent a stack,
and pushing and popping are simply
incrementing or decrementing a stack pointer.

### Some Comments

Many details about the implementation is still missing here.
We have discussed about how these primitive operations
can be implemented using procedures in the host language.
(e.g. `vector-set!` and `vector-ref`), but how we are going to
represent the types? For example, if a number is stored in the
`the-cars` vector, how can we distinguish a number
from a "memory address"?
A specification for type fields are missing, so for the few
exercises in future, we just simply introduce list primitives
from host language into this machine, pretending that we
have implemented them correctly.
