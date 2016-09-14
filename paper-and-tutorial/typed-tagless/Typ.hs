module Typ where

-- trying to replicate http://okmij.org/ftp/tagless-final/course/Typ.hs

{-
  the language of type representation.
  supporting just integers and functions.
  concrete instances are needed to give semantics
-}
class TSYM trepr where
    tint :: trepr Int
    tarr :: trepr a -> trepr b -> trepr (a -> b)

newtype ShowT a = ShowT String

instance TSYM ShowT where
    tint = ShowT "Int"
    tarr (ShowT a) (ShowT b) = ShowT $ "(" ++ a ++ "->" ++ b ++ ")"

viewTy :: ShowT a -> String
viewTy (ShowT s) = s
