module Except where

import qualified Control.Category as Cat
import Control.Arrow

newtype Except a b c = E (a b (Either String c))

arrE :: Arrow a => (i -> o) -> Except a i o
arrE f = E (arr (Right . f))

compE :: ArrowChoice arr => Except arr a b -> Except arr b c -> Except arr a c
compE (E f) (E g) = E $ f >>> right g >>> arr (\e -> case e of
                                                   Left msg -> Left msg
                                                   Right (Left msg) -> Left msg
                                                   Right (Right v) -> Right v)

instance ArrowChoice a => Cat.Category (Except a) where
    id = arrE id
    g . f = compE f g

instance ArrowChoice a => Arrow (Except a) where
    arr = arrE
    first (E f) = E $ (f *** arr id) >>> arr assoc
      where
        assoc :: (Either String x, d) -> Either String (x,d)
        assoc (e,d) = case e of
            Left l -> Left l
            Right v -> Right (v,d)
