{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module TypeCheck where

import Typ
import TTFdB

-- the data structure we'll be serializing from and to.
data Tree
  = Leaf String
  | Node String [Tree]
    deriving (Eq, Read, Show)

data DynTerm repr h = forall a. DynTerm (TQ a) (repr h a)

type VarName = String
data VarDesc t =
    VarDesc (TQ t) VarName

class Var gamma h | gamma -> h where
    findVar :: Semantics repr =>
               VarName -> gamma -> Either String (DynTerm repr h)

instance Var () () where
    findVar name _ = fail $ "Unbound variable: " ++ name

-- TODO: to allow this instance we need to have "UndecidableInstances",
-- I'm wondering what does it do?
instance Var gamma h => Var (VarDesc t, gamma) (t,h) where
    findVar name (VarDesc tr name', _) | name == name' = pure $ DynTerm tr z
    findVar name (_,gamma) = do
        DynTerm tr v <- findVar name gamma
        pure $ DynTerm tr (s v)

readT :: Tree -> Either String Typ
readT (Node "TInt" []) = pure $ Typ tint
readT (Node "TArr" [e1,e2]) = do
    Typ t1 <- readT e1
    Typ t2 <- readT e2
    pure $ Typ $ tarr t1 t2
readT tree = fail $ "Bad type expression: " ++ show tree

typecheck :: (Semantics repr, Var gamma h) =>
             Tree -> gamma -> Either String (DynTerm repr h)
typecheck (Node "Int" [Leaf str]) _ =
    case reads str of
        -- parse "str" as an Int, consuming it all.
        [(n,[])] -> pure (DynTerm tint (int n))
        _ -> fail $ "Bad Int literal: " ++ str
typecheck (Node "Var" [Leaf name]) gamma = findVar name gamma
-- TODO: more cases
typecheck e _ = fail $ "Invalid Tree: " ++ show e
