{-# LANGUAGE
    DataKinds
  , NoMonomorphismRestriction
  , ScopedTypeVariables
  , RankNTypes
  , RebindableSyntax
  , FlexibleContexts
  , ConstraintKinds
  , MultiParamTypeClasses
  , FlexibleInstances
  , KindSignatures
  , TypeOperators
  , GADTs
  , TypeFamilies
  , PolyKinds
  , UndecidableInstances
  #-}
module EffSys where

import Prelude hiding (return, pure, (>>=), (>>))
import Data.Monoid (Sum(..))
import GHC.Exts
import GHC.TypeLits
import Data.Proxy

class Effect (m :: k -> * -> *) where
    -- Unit together with Plus gives us a monoid that
    -- we have to define for each Effect instance.
    -- this basically specifies what to do on type level
    -- when we are composing effects together
    type Unit m :: k
    type Plus m (f :: k) (g :: k) :: k
    -- having this thing around makes it possible to add more constraints
    -- when we are composing effect together, the default definition is given
    -- by "Inv m f g = ()", which does not add any constraint at all.
    type Inv m (f :: k) (g :: k) :: Constraint
    type Inv m f g = ()
    pure :: a -> m (Unit m) a
    (>>=) :: Inv m f g => m f a -> (a -> m g b) -> m (Plus m f g) b

    (>>) :: Inv m f g => m f a -> m g b -> m (Plus m f g) b
    x >> y = x >>= (\_ -> y)

class Subeffect (m :: k -> * -> *) f g where
    sub :: m f a -> m g a

-- related: https://hackage.haskell.org/package/type-level-sets

data Set (n :: [*]) where
    Empty :: Set '[]
    Ext :: e -> Set s -> Set (e ': s)

-- one pass in a buble sort, by the end of "Pass", we will sure the
-- last element in the resulting list is the "largest"
-- undering the corresponding {Min,Max} notion
type family Pass (l :: [*]) :: [*] where
    Pass '[] = '[]
    Pass '[e] = '[e]
    -- nested pattern requires UndecidableInstances:
    -- https://downloads.haskell.org/~ghc/8.0.1-rc4/docs/html/users_guide/glasgow_exts.html#instance-declarations
    -- basically there are few rules that GHC uses to ensure instance resolution terminates
    -- with "UndecidableInstances", these rules will be bypassed so
    -- now we are responsible to make sure that instance resolution does terminates
    -- in this example the input is "(e : f : s)" and the recursive call is "(_ : s)" where
    -- "_" is one of either "e" or "f".
    -- so yes this does guarantee to be a "smaller problem" so this is safe.

    -- but this is a type family definition,
    -- how is this related to undecidable *instances* ?
    -- https://downloads.haskell.org/~ghc/8.0.1-rc4/docs/html/users_guide/glasgow_exts.html#type-family-decidability
    -- so on the RHS of the following definition:
    -- - (1) "(Pass ((Max e f) ': s))" is a type family application, and its argument contains
    --   another type family constructor "Max"
    -- - (2) also, for the same part, type variable e,f,s
    --   is not strictly smaller than e,f,s (on the LHS)
    -- - (3) this one is satisfied
    Pass (e ': f ': s) = Min e f ': (Pass ((Max e f) ': s))

-- Min and Max are sorting mechanism
type family Min (a :: k) (b :: k) :: k
type family Max (a :: k) (b :: k) :: k

type family Bubble l l' where
    -- note that here "l'" is used as a list length counter:
    -- when the counter reaches 0 ("'[]"), we stop applying it to "Pass"
    Bubble l '[] = l
    -- when the counter is non-zero (guaranteed by having (_ ': _) pattern
    -- we recursively deal with "Bubble l xs" first, and then apply a "Pass"
    -- to the resulting type
    Bubble l (x ': xs) = Pass (Bubble l xs)
{- say if we have a type level list of size 3, [a,b,c]:

   Bubble [a,b,c] [a,b,c]
=> Pass (Bubble [a,b,c] [b,c])
=> Pass (Pass (Bubble [a,b,c] [b]))
=> Pass (Pass (Pass (Bubble [a,b,c] [])))
=> Pass (Pass (Pass [a,b,c]))

so if the list length is n, we will have n passes applications "Pass"
-}

-- the interface to bubble sort, the output won't
-- always be correct if Bubble's two argument list mismatch in length
type Sort s = Bubble s s

-- TODO: the purpose of having both type-level and value-level stuff?
type Sortable s = Bubbler s s
class Bubbler s s' where
    bubble :: Set s -> Set s' -> Set (Bubble s s')

instance Bubbler s '[] where
    bubble s Empty = s

instance (Bubbler s t, Passer (Bubble s t)) => Bubbler s (e ': t) where
    bubble s (Ext _ t) = pass (bubble s t)

class Passer s where
    pass :: Set s -> Set (Pass s)

instance Passer '[] where
    pass Empty = Empty

instance Passer '[e] where
    pass (Ext e Empty) = Ext e Empty

instance (Passer ((Max e f) ': s), OrdH e f) => Passer (e ': f ': s) where
    pass (Ext e (Ext f s)) = Ext (minH e f) (pass (Ext (maxH e f) s))

class OrdH e f where
    minH :: e -> f -> Min e f
    maxH :: e -> f -> Max e f

type family Nub t where
    Nub '[] = '[]
    Nub '[e] = '[e]
    Nub (e ': e ': s) = Nub (e ': s)
    -- note that this case overlaps with the previous one.
    -- (with previous one "(e ': e ': s)" being more difficult to satisfy)
    -- only attempted when all other pattern have failed.
    Nub (e ': f ': s) = e ': Nub (f ': s)

class Nubable t where
    nub :: Set t -> Set (Nub t)

instance Nubable '[] where
    nub Empty = Empty

instance Nubable '[e] where
    nub (Ext x Empty) = Ext x Empty

{-
  not mentioned in the paper, but "{-# OVERLAPS #-}" seems to does the trick:
  OVERLAPS is both OVERLAPPABLE and OVERLAPPING.
  - OVERLAPPABLE: allowing a instance to be overlapped by others
  - OVERLAPPING: expect this instance to overlap others
-}
instance {-# OVERLAPS #-} (Nub (e ': f ': s) ~ (e ': Nub (f ': s)),
          Nubable (f ': s)) => Nubable (e ': f ': s) where
    nub (Ext e (Ext f s)) = Ext e (nub (Ext f s))

type AsSet s = Nub (Sort s)

asSet :: (Sortable s, Nubable (Sort s)) => Set s -> Set (AsSet s)
asSet x = nub (bsort x)

-- TODO: not sure whether this is correct, but it type checks.
bsort :: Bubbler s s => Set s -> Set (Sort s)
bsort s = bubble s s

type IsSet s = ((s ~ Nub (Sort s)) :: Constraint)

type family Append (s :: [*]) (t :: [*]) :: [*] where
    Append '[] t = t
    Append (x ': xs) ys = x ': Append xs ys

append :: Set s -> Set t -> Set (Append s t)
append Empty x = x
append (Ext e xs) ys = Ext e (append xs ys)

type Union s t = AsSet (Append s t)
type Unionable s t = (Sortable (Append s t), Nubable (Sort (Append s t)))

union :: (Unionable s t) => Set s -> Set t -> Set (Union s t)
union s t = nub (bsort (append s t))

-- "Subset s t" is a valid instance iff. s is a subset of t
class Subset s t where
    subset :: Set t -> Set s

instance Subset '[] t where
    subset _ = Empty

instance Subset s t => Subset (x ': s) (x ': t) where
    subset (Ext x xs) = Ext x (subset xs)

instance Subset s t => Subset s (any ': t) where
    subset (Ext _ xs) = subset xs

data (v :: Symbol) :-> (t :: *) = (Var v) :-> t
data Var (v :: Symbol) = Var

-- Writer monad by Effect typeclass
data Writer w a = Writer { runWriter :: (a, Set w) }

instance Effect Writer where
    -- constraint when composing two effects:
    -- both effects should contain a proper set
    type Inv Writer s t = (IsSet s, IsSet t, Unionable s t)
    type Unit Writer = '[]
    type Plus Writer s t = Union s t
    pure x = Writer (x, Empty)
    (Writer (a,w)) >>= k =
        let Writer (b,w') = k a
        in Writer (b, w `union` w')

-- compare type "a" and "b" (both are symbols)
-- and return "p" when "a" is not greater or "q" otherwise
type Select a b p q = Choose (CmpSymbol a b) p q

-- "Choose _ x y" is kind of like "if _ then x else y" on a type level
type family Choose (o :: Ordering) p q where
    Choose 'LT p q = p
    Choose 'EQ p q = p
    Choose 'GT p q = q

type instance Min (v :-> a) (w :-> b) =
    Select v w v w :-> Select v w a b
type instance Max (v :-> a) (w :-> b) =
    Select v w w v :-> Select v w b a

put :: Var v -> t -> Writer '[v :-> t] ()
-- it's "Writer ((), Ext v x Empty)" in the paper
-- and that doesn't type check and the arity doesn't make sense,
-- I guess this should be the original intention:
put v x = Writer ((), Ext (v :-> x) Empty)

instance (Monoid a, Nubable ((v :-> a) ': s)) =>
  Nubable ((v :-> a) ': (v :-> a) ': s) where
    nub (Ext (_ :-> a) (Ext (v :-> b) s)) =
      nub (Ext (v :-> (a `mappend` b)) s)

select :: forall j k a b. (Chooser (CmpSymbol j k)) => Var j -> Var k -> a -> b -> Select j k a b
select _ _ = choose (Proxy :: Proxy (CmpSymbol j k))

class Chooser (o :: Ordering) where
    choose :: Proxy o -> p -> q -> Choose o p q
instance Chooser 'LT where choose _ p _ = p
instance Chooser 'EQ where choose _ p _ = p
instance Chooser 'GT where choose _ _ q = q

instance (Chooser (CmpSymbol u v)) => OrdH (u :-> a) (v :-> b) where
    minH (u :-> a) (v :-> b) = Var :-> select u v a b
    maxH (u :-> a) (v :-> b) = Var :-> select u v b a

-- TODO: we eventually will cleanup the code and separate them into
-- different module, when that's done, we can expose varX, varY whatever
-- in the testing module I guess
-- GHC should be able to infer type signature for this:
test :: Writer '["x" :-> Sum Int, "y" :-> String] ()
test = do
    put varX (Sum (42 :: Int))
    put varY "saluton"
    put varX (Sum (58 :: Int))
    put varY "_mondo"
  where
    return = pure
    varX = Var :: (Var "x")
    varY = Var :: (Var "y")

-- can be infered, but the type looks messy.
test2 :: (IsSet f, Unionable f '["y" :-> String])
    => (Int -> Writer f t) -> Writer (Union f '["y" :-> String]) ()
test2 f = do
    -- run an existing effect "f" with argument 3
    _ <- f 3
    -- having an effect "y" :-> String on its own
    put varY "world."
  where
    return = pure
    varY = Var :: (Var "y")

-- actually very similar to Subset we have defined above
class Superset s t where
    superset :: Set s -> Set t

instance Superset '[] '[] where
    superset _ = Empty

instance (Monoid a, Superset '[] s) =>
  Superset '[] ((v :-> a) ': s) where
    superset _ = Ext (Var :-> mempty) (superset Empty)

instance Superset s t =>
  Superset ((v :-> a) ': s) ((v :-> a) ': t) where
    superset (Ext x xs) = Ext x (superset xs)

instance Superset s t => Subeffect Writer s t where
    sub (Writer (a,w)) = Writer (a,superset w :: Set t)

test' :: Num a => a -> Writer '["x" :-> Sum a, "y" :-> String] ()
test' (n :: a) = do
    put varX (Sum (42 :: a))
    put varY "hello "
    put varX (Sum (n :: a))
  where
    return = pure
    varX = Var :: (Var "x")
    varY = Var :: (Var "y")

-- we are extending effect "x" and "y" with a "z" (with "z" having a monoid support)
test3 :: Writer '["x" :-> Sum Int, "y" :-> String, "z" :-> Sum Int] ()
test3 = sub (test2 test')

{-
  as a side note to 4.1: Data.Monoid.Last does some thing similar:
  it is a Monoid that always take the last non-empty value as its final result,
  it has the behavior we are expecting:

  - "mappend x (Last Nothing)" is always just "x"
  - "mappend _ (Last (Just v))" always ignores its first argument and
    return its second one

  so Writer alone can do the job well already with just the Last Monoid,
  but what's important about "Update" effect is that the cell doesn't
  have to hold the value of same type: you can put in a value of "Int"
  and later decide to replace it with something of type "String" instead.
-}

-- as a GADT, we get the freedom of storing value of
-- whatever type possible by making the type argument to (lifted) "Maybe" abstract
data Eff (w :: Maybe *) where
    Put :: a -> Eff ('Just a)
    NoPut :: Eff 'Nothing

data Update w a = U { runUpdate :: (a, Eff w) }

instance Effect Update where
    -- type level effect composition:
    -- the last non-Nothing value wins
    type Unit Update = 'Nothing
    type Plus Update s 'Nothing = s
    type Plus Update s ('Just t) = 'Just t

    pure x = U (x, NoPut)
    (U (a,w)) >>= k = U (update w (runUpdate $ k a))

-- composing two effects in order, passing along whatever value
-- the second has
update :: Eff s -> (b, Eff t) -> (b, Eff (Plus Update s t))
update w (b, NoPut) = (b,w)
update _ (b, Put w') = (b, Put w')

-- since we have defined another "put" in the same file
putUpd :: a -> Update ('Just a) ()
putUpd x = U ((), Put x)

foo :: Update ('Just String) ()
foo = putUpd (42 :: Int) >> putUpd "hello"

data Reader s a = R { runReader :: Set s -> a }

instance Effect Reader where
    -- the following won't work, so there is a subtle different between
    -- the implementation of Writer and that of Reader, see comments below

    -- type Inv Reader s t = (IsSet s, IsSet t, Unionable s t)
    type Unit Reader = '[]
    type Plus Reader s t = Union s t

    pure x = R (\_ -> x)

    -- the following won't type check, the problem is that:
    -- - as an input to run the effect, we expect a full list of things
    --   we might read during the execution of a Reader,
    -- - but now we are composing two Readers together and it's totally possible
    --   that two Readers are expecting different sets of values to be read
    -- - so despite that "st" has more than enough thing to feed both readers
    --   we'll have to just take apart "st" and offer two readers exactly what they want.
    -- - comparing this with that of Writer, we can see the difference is in the variance
    --   of info we are carrying along the computation:
    --   - Writer writes, and the info to be carried are some sort of *output* of the effect.
    --   - Reader reads, and the info to be carried are some sort of *input* of the effect.
    -- - also I'll prefer renaming "split" to "dispatch" instead, as "split" reminds me
    --   of spliting a set of things into *disjoint* set, which is not the case for
    --   our implementation of Reader: in our case we look at the "things to be read"
    --   one by one, and distribute then to two Readers as they demand.

    -- (R e) >>= k = R (\st -> (runReader $ k (e st)) st)

-- direction: input "st", output "s" "t"
class Split s t st where
    split :: Set st -> (Set s, Set t)

instance Split '[] '[] '[] where
    split _ = (Empty, Empty)

-- distributing an element to both
instance (Split s t st) => Split (x ': s) (x ': t) (x ': st) where
    split (Ext x st) = (Ext x s, Ext x t)
      where
        (s,t) = split st

-- distributing to just "s" set
instance (Split s t st) => Split (x ': s) t (x ': st) where
    split (Ext x st) = (Ext x s, t)
      where
        (s,t) = split st

-- distributing to just "t" set
instance (Split s t st) => Split s (x ': t )(x ': st) where
    split (Ext x st) = (s, Ext x t)
      where
        (s,t) = split st

-- TODO: shouldn't we have something like:
-- instance (Split s t st) => Split s t (any : st)  ?
