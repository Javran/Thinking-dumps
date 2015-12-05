## Notes for Chapter 5

### Arrays, Shapes, and Indices

The data type for Repa arrays is `Array r sh e`:

* `e` is the element type
* `sh` indicates dimension (called `Shape`)
* `r` indicates how it's actually represented in memory

About dimension, its definition is like lists:

```haskell
data Z = Z
data tail :. head = tail :. head
```

An 3x5 array can be represented as `Z :. 3 :. 5` (3 rows, 5 columns).
Shapes and indices are constructed in the same way: `Z :. 3 :. 5` could
also mean a two-dimensional coordinate (Repa's indices are zero-based).

Note that in memory everything is just stored in linear space,
the shape is only useful for calculating the linear index given
a probably-high-dimensional one. We can change from an array that has 3 rows
to one that has 5 rows without even copying.

Representation `U` means an unboxed array (those that cannot be delayed,
e.g. primitive integers), and `D` means a delayed representation.
A delayed representation allows fusion to happen:

Consider `map g . map f`, it could traverse twice: one for `f`, and another for `g`.
Now if we change it to `map (g . f)`, the result should be the same but
this time we only need to traverse the data structure for applying `g . f`,
which is more efficient.

There are other techniques of fusion, with delayed representation,
we have a chance to find out those things that can be fused, the performence thus
can be improved.

### Example: Computing Shortest Paths

* For scenarios like using dynamic programming, values depend on each other,
we should better use `computeS` to force a manifest represetation rather
than allowing a chain of functions to build up

* Optimizations work better when all the code is visible to the compiler, if
performance is of important, you can try writing out explicit loops rather than
using other high-order library functions.

* It is a good practice to put `BangPatterns` on the arguments to those iterative
loops, which would help Repa to further optimize the code

* Turning on `LLVM` backend (`-fllvm`) sometimes could help to get better performance.

* An alternative to `computeS` is called `computeP`, which has almost the same
signature except that it has to be run in a monad. The purpose of this function
is to allow us speed up computation by puting tasks over available cores.

* In Repa, you can also do fold by using `foldS` and `foldP`, the former one is like `computeS`
and the latter one does not guarantee to fold elements in strict left-to-right order.
In addition to all requirements `foldS` might have, `foldP` requires its arguments `f :: a -> a -> a`
to be associative.
