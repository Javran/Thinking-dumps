module Interp where

import Control.Arrow
import Data.Maybe

data Exp
  = Var String
  | Add Exp Exp

type Val = Int

type Env = [(String,Val)]

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> arr (uncurry op)

eval :: Arrow a => Exp -> a Env Val
eval (Var s) = arr (fromJust . lookup s)
eval (Add e1 e2) = liftA2 (+) (eval e1) (eval e2)
