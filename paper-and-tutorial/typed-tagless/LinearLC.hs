{-# LANGUAGE
    NoMonomorphismRestriction
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}
module LinearLC where

newtype F a = F a
data U = Used

class LSemantics repr where
    -- notice that unlike "repr h a" which is what we usually see,
    -- we now have two "environments", "repr hi ho a", which gives us
    -- distinction between "input environment" and "output environment"

    -- using a constant does not change our environment
    int :: Int -> repr hi hi Int
    -- "add <a> <b>" first traverses "<a>" with "hi" as input env and "h" as intermedia env
    -- and then "<b>" with "h" as input env and "ho" output env
    -- forming an expression that takes as input env "hi" and turns it into "ho"
    add :: repr hi h Int -> repr h ho Int -> repr hi ho Int

    -- a reference about the variable consumes that variable from environment.
    z :: repr (F a, h) (U, h) a
    -- "s <e>" hides the first element for "<e>"
    s :: repr hi ho a -> repr (any,hi) (any,ho) a
    -- actually this case is very similar to "add": for "app <a> <b>",
    -- we first traverse "<a>" then "<b>", and finally do the application
    app :: repr hi h (a->b) -> repr h ho a -> repr hi ho b
