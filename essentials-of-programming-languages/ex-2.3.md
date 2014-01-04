* Show that every number has infinitely many representations in this system.

Notice that the representation for zero can be `(diff (one) (one))`, and for any representation of any number `x`,
we can always make a new representation by performing the transformation:
`x => (diff x zero) => (diff x (diff (one) (one)))`.

So the representation of any number is infinite.

* Implementation

See `ex-2.3.rkt`

* `diff-tree-plus`

See `ex-2.3.rkt`
