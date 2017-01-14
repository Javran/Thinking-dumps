{-# LANGUAGE ScopedTypeVariables #-}
module ListMap where

import qualified Control.Category as Cat
import Control.Arrow

newtype ListMap i o = LM ([i] -> [o])

arrLM :: (i -> o) -> ListMap i o
arrLM f = LM (f <$>)

composeLM :: ListMap a b -> ListMap b c -> ListMap a c
composeLM (LM f) (LM g) = LM $ g . f

{-
  ListMap is Category for sure: in fact it's just
  an instance of function composition acting on lists.
-}
instance Cat.Category ListMap where
    id = arrLM id
    g . f = composeLM f g

{-
  however, ListMap is not an instance of Arrow:
  the problem comes when we are trying to write an impl
  for "first":

  First notice that  ListMap is just a general function of transforming
  a list of type `[i]` into another one of type `[o]`,
  (in other words, it's just a function of type `[i] -> [o]` in disguise).

  So if we are given as argument a `ListMap i o`, we are supposed to construct
  `ListMap (i,b) (o,b)`, which is just a function of type `[(i,b)] -> [(o,b)]`.
  And the only sensible route I can think of is to first turn `[(i,b)]` into `[i]`,
  which allows the argument function to be applied on it to get `[o]`, and then somehow
  recover `[(o,b)]` from it.

  However, a function `[i] -> [o]` is a general transformation,
  which does not allow us to perform element-wise operations on it.
  This means while we can always turn `[(i,b)]` into `[i]` because lists are functors,
  we cannot find a way to recover the information kept by `[(_,b)]` part.

-}


{-
  In the following attempted implemnentation, the hole is exactly the place
  we can't find a way to fill: while we can get rid of "(,b)" parts for "f"
  to be applicable, we cannot recover it afterwards:
-}
firstLM :: forall i o b. ListMap i o -> ListMap (i,b) (o,b)
firstLM (LM f) = LM $ \(xs :: [(i,b)]) ->
    let xs' :: [i]
        xs' = fst <$> xs
        ys' :: [o]
        ys' = f xs'
    in (_ ys' :: [(o,b)])
