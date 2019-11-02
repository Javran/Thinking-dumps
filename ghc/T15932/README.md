Ticket: https://gitlab.haskell.org/ghc/ghc/issues/15932

For simplicity let's first begin with investigating `DeriveFunctor`.

`example/` contains the sample code I'm working with.

```bash
$ ghc -fhpc Cov.hs test.hs -ddump-simpl -fforce-recomp
```

A single run of the compiled binary gives:

```
Tix [ TixModule "Main" 1241602044 36 [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1], TixModule "Cov" 2046546664 10 [1,0,1,1,1,1,1,1,1,1]]
```

The missing tix is `hpc<Cov,1>`:

```
-- RHS size: {terms: 4, types: 9, coercions: 0, joins: 0/0}
$c<$1_r5oV :: forall b a. a -> (Int, b) -> (Int, a)
[GblId]
$c<$1_r5oV
  = \ (@ b_a5l5) (@ a_a5l4) ->
      (hpc<Cov,1> <$ @ ((,) Int) (GHC.Base.$fFunctor(,) @ Int))
        @ a_a5l4 @ b_a5l5
```

Now adding use of `<$` will give us `100%` coverage. I suspect `fmap` of the derived version somehow
missed a tick somewhere.

Now let's see what we can do with `Cov2`, both derived and manual version is included in
one single unit.

TODO: (<$) and fmap

Derived:

```
-- RHS size: {terms: 5, types: 7, coercions: 0, joins: 0/0}
$c<$1_r19v :: forall a b. a -> F b -> a
[GblId, Arity=2, Caf=NoCafRefs, Unf=OtherCon []]
$c<$1_r19v
  = \ (@ a_atn) (@ b_ato) (z_at6 :: a_atn) _ [Occ=Dead] ->
      hpc<Cov2,1> z_at6
```

Manual:

```
-- RHS size: {terms: 4, types: 7, coercions: 0, joins: 0/0}
$c<$_r19u :: forall a b. a -> G b -> G a
[GblId]
$c<$_r19u
  = \ (@ a_atN) (@ b_atO) ->
      GHC.Base.$dm<$ @ G Cov2.$fFunctorG @ a_atN @ b_atO
```

For fmap:

Derived:

```
-- RHS size: {terms: 6, types: 8, coercions: 2, joins: 0/0}
$cfmap1_r19w :: forall a b. (a -> b) -> F a -> b
[GblId, Arity=2, Caf=NoCafRefs, Unf=OtherCon []]
$cfmap1_r19w
  = \ (@ a_ate)
      (@ b_atf)
      (f_at4 :: a_ate -> b_atf)
      (ds_d195 :: F a_ate) ->
      hpc<Cov2,0>
      f_at4 (ds_d195 `cast` (Cov2.N:F[0] <a_ate>_R :: F a_ate ~R# a_ate))
```

Manual:

```
$cfmap_r18W :: forall a b. (a -> b) -> G a -> b
[GblId, Arity=2, Caf=NoCafRefs, Unf=OtherCon []]
$cfmap_r18W
  = \ (@ a_atD)
      (@ b_atE)
      (f_asa :: a_atD -> b_atE)
      (ds_d197 :: G a_atD) ->
      hpc<Cov2,5>
      hpc<Cov2,4>
      hpc<Cov2,3>
      f_asa
        ((hpc<Cov2,2> ds_d197)
         `cast` (Cov2.N:G[0] <a_atD>_R :: G a_atD ~R# a_atD))
```



(following are old notes that needs some cleanup)


In short, stuff like

```
Cov.$fFunctorF = GHC.Base.C:Functor @ F $cfmap_a5k7 $c<$_a5l2`
```

should have coverage tick attached:

```
Cov.$fFunctorF = hpc<Cov,X> ...
```

Starting point is `deSugar/Coverage.hs`:

- `addTicksToBinds`:  the pass in Coverage.hs runs *after* generating the derived code.
- `TcGenDeriv`: code generators for things like `deriving (Eq)`
- `TcGenFunctor`: for deriving Functor

Minimal example:

```haskell
{-# LANGUAGE DeriveFunctor #-}
module Cov where

newtype F a = F a deriving Functor
```

Few more ideas:

- looks like there is exactly one pass where ticks get added: `HpcTicks`,
  so even it's `foldr` to allow multiple passes, we only need to look for one.
- Sounds like `addTickLHsBind` is responsible for adding ticks.

But now I'm not sure `Cov.$fFunctorF` is the right place to add coverage.

Even if I implement the instance myself:

```haskell
module Cov where
newtype F a = F a
instance Functor F where
  fmap f (F a) = F (f a)
```

The result looks like:

```
Cov.$fFunctorF [InlPrag=NOUSERINLINE CONLIKE] :: Functor F
[LclIdX[DFunId],
 Unf=DFun: \ -> GHC.Base.C:Functor TYPE: F $cfmap_ask $c<$_asw]
Cov.$fFunctorF = GHC.Base.C:Functor @ F $cfmap_ask $c<$_asw
```

So perhaps what I initially have in mind is wrong?
