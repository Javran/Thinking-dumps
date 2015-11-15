## Notes for Chapter 2

### Weak Head Normal Form

* Things in WHNF exposes at least it's outermost constructor
    - e.g. for `Maybe` it's `Nothing` or `Just`
    - can still contain thunks (e.g. `Just _`)
    - in contrast, there is Normal Form (NF), in which all fields has to be fully evaluated
        + WHNF but not NF: `Just _`, `[_,_,_]`, `1 :: 2 :: _`, `_ :: _`
        + not WHNF: `_`

* GHCi command `:sprint` can be used to examine the current known
structure of a Haskell expression (noninvasively)

* `seq a b` forces `a` to be evaluated before returning `b`, thus at
the time when we need `b`, we know `a` must have been evaluated (to at least WHNF)
