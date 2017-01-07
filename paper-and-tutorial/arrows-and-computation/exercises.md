# Exercise 1

Arrow instances for `Reader` and `Writer`.

See `Reader.hs` and `Writer.hs`

# Exercise 2

# Exercise 3

Arrow instance of `Stream`

See `Stream.hs`

# Exercise 4

See `Exercise4.hs`.

# Exercise 5

Composition:

```
arr (cross (>>> h) id) >>> app
=> (cross (>>> h) id) >>> app
=> first (h .) >>> \(f,c) -> f c
=> \(f',c) -> (h . f') c
```

```
app >>> h
=> \(f,c) -> f c >>> h
=> \(f,c) -> h (f c)
=> \(f,c) -> (h . f) c
```

Reduction:

With arrow being `->`, we have:

```
mkPair b = arr (\c -> (b,c))
=> mkPair b = \c -> (b,c)
=> mkPair b c = (b,c)
=> mkPair = (,)
```

Now:

```
arr (cross mkPair id) >>> app
=> cross mkPair id >>> app
=> \(x,y) -> (mkPair x,y) >>> \(x,y) -> x y
=> \(x,y) -> (mkPair x) y
=> \(x,y) -> (x,y)
=> id === arr id
```

Extensionality:

```
mkPair f >>> app
=> \x -> (f,x) >>> \(x,y) -> x y
=> \x -> f x
=> f
```

# Exercise 6

Show that extensionality axiom fails for the given definition on Auto.

See `Automata.hs`.

# Exercise 7

`ArrowChoice` instance for `NonDet`, `State` and `StreamMap`.

See `NonDet.hs`, `State.hs` and `StreamMap.hs`.

# Exercise 8

Show that the equation `(f ||| g) >>> h = (f >>> h) ||| (g >>> h)` fails
for `Auto` and `StreamMap`.

For `Auto`: See `result1` and `result2` of `Automata.hs`, which gives an example
that LHS and RHS of the equation is different.

(TODO)

Let's try expanding both side of the equation first:

```
LHS:
(f ||| g) >>> h
=> (f +++ g) >>> arr untag >>> h
=> left f >>> right g >>> arr untag >>> h
```

```
RHS:
(f >>> h) ||| (g >>> h)
=> ((f >>> h) +++ (g >>> h)) >>> arr untag
=> left (f >>> h) >>> right (g >>> h) >>> arr untag
```

Now let's say `f = g = id`, so:
```
LHS:
=> arr untag >>> h
RHS:
=> left h >>> right h >>> arr untag
```

See if we can find some clue by expanding this further for `Auto` and `StreamMap`.o

# Exercise 9

Implement instance `ArrowChoice a => Arrow (Except a)`.

See `Except.hs`

# Exercise 10

# Exercise 11

# Exercise 12

# Exercise 13

Translate `genSym`.

See `genSym1` and its comments of `State.hs`

# Exercise 14

# Exercise 15

# Exercise 16

# Exercise 17

Sorting using `bisort`.

See `sort` and `sort1` of `Hom.hs`.

I still have no clue despite that I have implemented it.
The biggest problem is that we don't really know what `bisort` does.

# Exercise 18

Definition of `StateT` without arrow notation.

See `StateT.hs`.

# Exercise 19

Arrow instance of `AutoFunctor`.

See `AutoFunctor.hs`.
