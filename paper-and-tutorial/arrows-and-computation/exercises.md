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

For `StreamMap`, it depends on the implementation of `ArrowChoice`.
See comments in `instance ArrowChoice StreamMap`.

# Exercise 9

Implement instance `ArrowChoice a => Arrow (Except a)`.

See `Except.hs`

# Exercise 10

# Exercise 11

Define `ArrowLoop` for `StreamMap`.

(TODO)

See `Stream.hs`. The implementation is done but we need something to
confirm that it works.

# Exercise 12

Prove that `loop (first f) = f`.

Left tightening says:

```
loop (first h) >>> f = h >>> loop f
```

when `f = arr id`, this becomes:

```
loop (first h) = h >>> loop (arr id)
```

Now that according to extension law:

```
loop (arr f) = arr (trace f)
```

We know:

```
loop (first h) = h >>> loop (arr id)
=> loop (first h) = h >>> arr (trace id)
```

```
trace id = \b -> let (c,d) = (b,d) in c
=> trace id = \b -> let (c,d) = (b,d) in b
=> trace id = \b -> b
```

Therefore:

```
loop (first h) = h >>> arr (trace id)
=> loop (first h) = h >>> arr id
=> loop (first h) = h
```

And this is exactly what we are trying to prove.

# Exercise 13

Translate `genSym`.

See `genSym1` and its comments of `State.hs`

# Exercise 14

Prove that when both translations of `proc p -> f -< a` are possible,
they are equal:

- `proc p -> f -< a => arr (\p -> a) >>> f` (when p and f does not have common free variables)
- `proc p -> f -< a => arr (\p -> (f,a)) >>> app`

Answer:

Because both translations are possible, `p` and `f` must not have common free variables.
So we can deal with them independently.

By extensionality of `ArrowApply`:

```
arr (\p -> a) >>> f
=> arr (\p -> a) >>> mkPair f >>> app
=> arr (\p -> a) >>> arr (\c -> (f,c)) >>> app
=> arr ((\p -> a) >>> (\c -> (f,c)) >>> app
=> arr (\p -> (f,a)) >>> app
```

and this is the conclusion we are looking for.

# Exercise 15

Suggest a translation for the form:

```
if <exp> then <cmd> else <cmd>
```

Answer:

Let's also name the input pattern:

```
proc p -> if exp then cmd1 else cmd2
```

And to make use of `ArrowChoice` instance, we might consider tagging things differently
depending on `exp`:

```
arr (\p -> if exp then Left ?1 else Right ?2) >>> (?3 ||| ?4)
```

Now for `?1` and `?2`, the best we can do to just pass the whole pattern
so `cmd1` and `cmd2` will have full access to it:

```
arr (\p -> (if exp then Left else Right) p) >>> (?3 ||| ?4)
```

Therefore for `?3` and `?4`, we just run the command:

```
arr (\p -> (if exp then Left else Right) p) >>> (proc p -> cmd1 ||| proc p -> cmd2)
```

And this is the translation we end up with.

About `case` expression translation: basically `ArrowChoice` is built on top of pairs, and
if there are more than 2 cases to consider, we turn them into nested pairs.
I'm not sure whether there is a similar way of translating `case` expressions
without tons of machinery for turning each case from or to some nested pair structures.

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
