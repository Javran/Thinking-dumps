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
  for "first", while its argument allows us to transform
  a list of type "[i]" into some list of type "[o]",
  we don't really know how to transform from an arbitrary "[(i,b)]" into "[(o,b)]"!
  (TODO) for now I'm not sure how to persuade myself of this,
  but the intuition is: ListMap is just a general function of transforming
  one list into another, so we are not supposed to perform element-wise operations.
  however, since we need to implement "first" for ListMap to become a valid Arrow,
  we have to element-wise-ly detach and attach extra data so that keeps intact,
  which is impossible with a general function
-}

{-
firstLM :: ListMap i o -> ListMap (i,d) (o,d)
firstLM (LM f) = LM $ \xs -> _
-}
