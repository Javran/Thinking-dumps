{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
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
readT tree = Left $ "Bad type expression: " ++ show tree

typecheck :: (Semantics repr, Var gamma h) =>
             Tree -> gamma -> Either String (DynTerm repr h)
typecheck (Node "Int" [Leaf str]) _ =
    case reads str of
        -- parse "str" as an Int, consuming it all.
        [(n,[])] -> pure (DynTerm tint (int n))
        _ -> Left $ "Bad Int literal: " ++ str
typecheck (Node "Add" [e1, e2]) gamma = do
    DynTerm (TQ t1) d1 <- typecheck e1 gamma
    DynTerm (TQ t2) d2 <- typecheck e2 gamma
    case (asInt t1 d1, asInt t2 d2) of
        (Just t1', Just t2') -> pure (DynTerm tint $ add t1' t2')
        (Nothing, _) -> Left $ "Bad type of a left summand " ++ viewTy t1
        (_, Nothing) -> Left $ "Bad type of a right summand " ++ viewTy t2
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

txView :: Either String (DynTerm S ()) -> String
txView t = case t of
    Right (DynTerm _ t') -> view t'
    Left err -> "Error: " ++ err

tx1, tx2, tx3 :: Semantics repr => Either String (DynTerm repr ())

-- unbound
tx1 =
    typecheck
      (Node "Var" [Leaf "x"]) ()

-- should pass
tx2 =
    typecheck
      (Node "Lam" [Leaf "x", Node "TInt" [],
                   Node "Var" [Leaf "x"]]) ()

-- should pass
tx3 =
    typecheck
      (Node "Lam" [Leaf "x", Node "TInt" [],
                   Node "Lam" [Leaf "y", Node "TInt" [],
                               Node "Add" [Node "Int" [Leaf "10"],
                                           Node "Var" [Leaf "x"]]]]) ()

newtype CL h a = CL { unCL :: forall repr. Semantics repr => repr h a }

instance Semantics CL where
    -- TODO: seems here it's difficult to simplify
    -- the following definition to "int = CL . int"
    -- need to find out why
    int x = CL (int x)
    add e1 e2 = CL (add (unCL e1) (unCL e2))
    z = CL z
    s v = CL (s (unCL v))
    lam e = CL (lam (unCL e))
    app e1 e2 = CL (unCL e1 `app` unCL e2)

tcEvalView :: Tree -> Either String (String, String)
tcEvalView tree = do
    DynTerm (tr :: TQ a) d <- typecheck tree ()
    -- make it explicit that this is polymorphic indeed
    let d' = unCL d :: forall repr. Semantics repr => repr () a
    -- show type and the expression itself
    -- (the type signature is optional, just to make things more explicit to see)
    pure (showAs tr (eval (d' :: R () a)), view (d' :: S () a))
