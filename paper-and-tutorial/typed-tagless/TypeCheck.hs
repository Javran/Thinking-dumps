{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}
module TypeCheck where

import Typ
import TTFdB

-- the data structure we'll be serializing from and to.
data Tree
  = Leaf String
  | Node String [Tree]
    deriving (Eq, Read, Show)

-- DynTerm is a pair of quantified type representation, and a representation
-- of an expression of that type
data DynTerm repr h = forall a. DynTerm (TQ a) (repr h a)

type VarName = String
data VarDesc t =
    VarDesc (TQ t) VarName

class Var gamma h | gamma -> h where
    findVar :: Semantics repr =>
               VarName -> gamma -> Either String (DynTerm repr h)

instance Var () () where
    -- not sure why, but a use of "fail" here lead to run time exception
    -- instead of capturing the exception inside, so I guess a "Left" will do
    -- the right thing.
    findVar name _ = Left $ "Unbound variable: " ++ name

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
        _ -> Left $ "Bad Int literal: " ++ str
typecheck (Node "Var" [Leaf name]) gamma =
    -- relying on instances of "Var gamma h" to resolve this.
    findVar name gamma
typecheck (Node "Lam" [Leaf name, etyp, ebody]) gamma = do
    -- lam (variable "name" of type "etyp") (expression body "ebody")
    Typ ta <- readT etyp
    -- parse and typecheck body of the function using extended "gamma"
    DynTerm tbody body <- typecheck ebody (VarDesc ta name, gamma)
    pure (DynTerm (tarr ta tbody) (lam body))
typecheck (Node "App" [e1, e2]) gamma = do
    -- typecheck "d1" of type "t1", and "d2" of type "t2"
    DynTerm (TQ t1) d1 <- typecheck e1 gamma
    DynTerm (TQ t2) d2 <- typecheck e2 gamma
    -- destruct t1 exposing the proof of equality
    case t1 of
        AsArrow _ arrCast ->
            case arrCast of
                Nothing -> Left $ "operator type is not an arrow: " ++ viewTy t1
                -- on successful destruction
                Just ((ta,tb),equT1ab) ->
                    case safeGCast (TQ t2) d2 ta of
                        Just da ->
                            pure $ let df = equCast equT1ab d1
                                   in DynTerm tb (app df da)
                        _ -> Left (unwords [ "Bad types of the application:"
                                           , viewTy t1
                                           , "and"
                                           , viewTy t2
                                           ])
typecheck e _ = Left $ "Invalid Tree: " ++ show e

txView t = case t of
    Right (DynTerm _ t') -> view t'
    Left err -> "Error: " ++ err

tx1 = typecheck (Node "Var" [Leaf "x"]) ()
