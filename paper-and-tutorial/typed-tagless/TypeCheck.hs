{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, LiberalTypeSynonyms #-}
module TypeCheck where

import Typ
import TTFdB

{-# ANN module "HLint: ignore Eta reduce" #-}

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
readT (Node "TBool" []) = pure $ Typ tbool
readT (Node "TArr" [e1,e2]) = do
    Typ t1 <- readT e1
    Typ t2 <- readT e2
    pure $ Typ $ tarr t1 t2
readT tree = Left $ "Bad type expression: " ++ show tree

-- Data.Function.fix does not work here.
-- not sure the exact problem, but most likely it has something to do with
-- type quantification.
typecheck :: forall repr gamma h. (Semantics repr, Var gamma h) => TypeCheck repr gamma h
typecheck = typecheckExt typecheck

type TypeCheck repr gamma h = Tree -> gamma -> Either String (DynTerm repr h)
type OpenRecursive a = a -> a

typecheckExt :: forall repr. (Semantics repr) =>
                OpenRecursive (forall gamma h. Var gamma h => TypeCheck repr gamma h)
typecheckExt _ (Node "Int" [Leaf str]) _ =
    case reads str of
        -- parse "str" as an Int, consuming it all.
        [(n,[])] -> pure (DynTerm tint (int n))
        _ -> Left $ "Bad Int literal: " ++ str
typecheckExt self (Node "Add" [e1, e2]) gamma = do
    DynTerm (TQ t1) d1 <- self e1 gamma
    DynTerm (TQ t2) d2 <- self e2 gamma
    case (asInt t1 d1, asInt t2 d2) of
        (Just t1', Just t2') -> pure (DynTerm tint $ add t1' t2')
        (Nothing, _) -> Left $ "Bad type of a left summand " ++ viewTy t1
        (_, Nothing) -> Left $ "Bad type of a right summand " ++ viewTy t2
typecheckExt _ (Node "Var" [Leaf name]) gamma =
    -- relying on instances of "Var gamma h" to resolve this.
    findVar name gamma
typecheckExt self (Node "Lam" [Leaf name, etyp, ebody]) gamma = do
    -- lam (variable "name" of type "etyp") (expression body "ebody")
    Typ ta <- readT etyp
    -- parse and typecheck body of the function using extended "gamma"
    DynTerm tbody body <- self ebody (VarDesc ta name, gamma)
    pure (DynTerm (tarr ta tbody) (lam body))
typecheckExt self (Node "App" [e1, e2]) gamma = do
    -- typecheck "d1" of type "t1", and "d2" of type "t2"
    DynTerm (TQ t1) d1 <- self e1 gamma
    DynTerm (TQ t2) d2 <- self e2 gamma
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
typecheckExt _ e _ = Left $ "Invalid Tree: " ++ show e

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

newtype CL h a = CL
  { unCL :: forall repr. ( Semantics repr
                         , SemanticsMul repr
                         , SemanticsBool repr) => repr h a }

{-
  for me it seems the purpose of having "CL" is to keep it polymorphic
  so that more than one "more specific" type can match it and do things
  accordingly. (see tcEvalView for example, in which destructing "d :: CL _ _"
  yields "d :: forall repr. Semantics repr => repr () a" which can then
  be matched against both "view" and "eval" and down to specific types
-}
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

-- modified to accept "mul", this should be a superset of the existing
-- language, and old code should work as expected
tcEvalView :: Tree -> Either String (String, String)
tcEvalView tree = do
    DynTerm (tr :: TQ a) d <- typecheckBool tree ()
    -- make it explicit that this is polymorphic indeed
    let d' = unCL d :: forall repr. ( Semantics repr
                                    , SemanticsMul repr
                                    , SemanticsBool repr) => repr () a
    -- show type and the expression itself
    -- (the type signature is optional, just to make things more explicit to see)
    pure (showAs tr (eval (d' :: R () a)), view (d' :: S () a))

class SemanticsMul repr where
    mul :: repr h Int -> repr h Int -> repr h Int

instance SemanticsMul R where
    mul e1 e2 = R $ \h -> unR e1 h * unR e2 h

instance SemanticsMul S where
    mul e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ "*" ++ unS e2 h ++ ")"

instance SemanticsMul CL where
    -- it's the same as using "unCL" to destruct it
    -- just want to try out.
    mul (CL e1) (CL e2) = CL (e1 `mul` e2)

{-
-- because CL has contained "Semantics repr" constraint inside of it,
-- the following one is not going to work...
instance SemanticsMul CL where
    mul e1 e2 = CL (unCL e1 `mul` unCL e2)
-- unless we modify the constraint inside of CL.
-}
typecheckMulExt :: forall repr. (Semantics repr, SemanticsMul repr) =>
                OpenRecursive (forall gamma h. Var gamma h => TypeCheck repr gamma h)
typecheckMulExt self (Node "Mul" [e1, e2]) gamma = do
    DynTerm (TQ t1) d1 <- self e1 gamma
    DynTerm (TQ t2) d2 <- self e2 gamma
    case (asInt t1 d1, asInt t2 d2) of
        (Just t1', Just t2') -> pure (DynTerm tint $ mul t1' t2')
        (Nothing, _) -> Left $ "Bad type of a left summand " ++ viewTy t1
        (_, Nothing) -> Left $ "Bad type of a right summand " ++ viewTy t2
typecheckMulExt self e gamma = typecheckExt self e gamma

typecheckMul :: forall repr gamma h. (Semantics repr, SemanticsMul repr, Var gamma h)
                => TypeCheck repr gamma h
typecheckMul = typecheckMulExt typecheckMul

class SemanticsBool repr where
    bool :: Bool -> repr h Bool
    leq :: repr h Int -> repr h Int -> repr h Bool
    bNot :: repr h Bool -> repr h Bool
    -- implement "bNot" and at least one of "bAnd" or "bOr"
    -- to have a full implementation
    bAnd :: repr h Bool -> repr h Bool -> repr h Bool
    bAnd e1 e2 = bNot $ bNot e1 `bOr` bNot e2
    bOr :: repr h Bool -> repr h Bool -> repr h Bool
    bOr e1 e2 = bNot $ bNot e1 `bAnd` bNot e2

    if_ :: repr h Bool -> repr h a -> repr h a -> repr h a

instance SemanticsBool S where
    bool x = S $ \_ -> show x
    leq eL eR = S $ \h ->
        "(" ++ unS eL h ++ " <= " ++ unS eR h ++ ")"
    bNot e = S $ \h -> "(not " ++ unS e h ++ ")"
    bAnd e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " && " ++ unS e2 h ++ ")"
    bOr e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " || " ++ unS e2 h ++ ")"
    if_ eCond eThen eElse = S $ \h ->
        "if " ++ unS eCond h ++
        " then " ++ unS eThen h ++
        " else " ++ unS eElse h

instance SemanticsBool R where
    bool x = R $ const x
    leq eL eR = R $ \h -> unR eL h <= unR eR h
    bNot e = R $ \h -> not (unR e h)
    bAnd e1 e2 = R $ \h -> unR e1 h && unR e2 h
    bOr e1 e2 = R $ \h -> unR e1 h || unR e2 h
    if_ eCond eThen eElse = R $ \h ->
        let selected = if unR eCond h then eThen else eElse
        in unR selected h

instance SemanticsBool CL where
    bool x = CL (bool x)
    leq (CL e1) (CL e2) = CL (e1 `leq` e2)
    bNot (CL e) = CL (bNot e)
    bAnd (CL e1) (CL e2) = CL (e1 `bAnd` e2)
    bOr (CL e1) (CL e2) = CL (e1 `bOr` e2)
    if_ (CL eC) (CL eT) (CL eE) = CL (if_ eC eT eE)

typecheckBoolExt :: forall repr.
                    ( Semantics repr
                    , SemanticsMul repr
                    , SemanticsBool repr ) =>
                OpenRecursive (forall gamma h. Var gamma h => TypeCheck repr gamma h)
typecheckBoolExt _ (Node "Bool" [Leaf str]) _ = do
    result <- case str of
        "t" -> Right True
        "f" -> Right False
        _ -> Left "boolean literal must be one of 't' or 'f'"
    pure (DynTerm tbool (bool result))
typecheckBoolExt self (Node "Leq" [e1, e2]) gamma = do
    DynTerm (TQ t1) d1 <- self e1 gamma
    DynTerm (TQ t2) d2 <- self e2 gamma
    case (asInt t1 d1, asInt t2 d2) of
        (Just t1', Just t2') -> pure (DynTerm tbool $ leq t1' t2')
        (Nothing, _) -> Left $ "Bad type of a left summand " ++ viewTy t1
        (_, Nothing) -> Left $ "Bad type of a right summand " ++ viewTy t2
typecheckBoolExt self (Node "Not" [e]) gamma = do
    DynTerm (TQ t) d <- self e gamma
    case asBool t d of
        Just t' -> pure (DynTerm tbool $ bNot t')
        Nothing -> Left $ "Bad type of the subexpression: " ++ viewTy t
typecheckBoolExt self (Node "And" [e1, e2]) gamma = do
    DynTerm (TQ t1) d1 <- self e1 gamma
    DynTerm (TQ t2) d2 <- self e2 gamma
    case (asBool t1 d1, asBool t2 d2) of
        (Just t1', Just t2') -> pure (DynTerm tbool $ bAnd t1' t2')
        (Nothing, _) -> Left $ "Bad type of a left summand " ++ viewTy t1
        (_, Nothing) -> Left $ "Bad type of a right summand " ++ viewTy t2
typecheckBoolExt self (Node "Or" [e1, e2]) gamma = do
    DynTerm (TQ t1) d1 <- self e1 gamma
    DynTerm (TQ t2) d2 <- self e2 gamma
    case (asBool t1 d1, asBool t2 d2) of
        (Just t1', Just t2') -> pure (DynTerm tbool $ bOr t1' t2')
        (Nothing, _) -> Left $ "Bad type of a left summand " ++ viewTy t1
        (_, Nothing) -> Left $ "Bad type of a right summand " ++ viewTy t2
typecheckBoolExt self (Node "If" [eCond, eThen, eElse]) gamma = do
    DynTerm (TQ tCond) dCond <- self eCond gamma
    -- eagerly make sure the condition is indeed a bool
    case asBool tCond dCond of
        Nothing -> Left $ "Bad type of condition expr: " ++ viewTy tCond
        Just tC -> do
            DynTerm (TQ tThen) dThen <- self eThen gamma
            DynTerm (TQ tElse) dElse <- self eElse gamma
            -- by safeGCast dThen to get dThen'
            -- we are persuading the type system "dThen'" and "dElse" is of the same type
            -- (despite that "dThen"'s type might not unify
            -- with that of "dElse" because of the quantification)
            case safeGCast (TQ tThen) dThen (TQ tElse) of
                Just dThen' -> pure (DynTerm tElse (if_ tC dThen' dElse))
                Nothing -> Left $ "Type mismatch on 2 branches: "
                               ++ viewTy tThen
                               ++ " vs. " ++ viewTy tElse
-- what we are extending is really important here, "typecheckExt" extends
-- the original one while "typecheckMulExt" extends the previous one with "Mul"
typecheckBoolExt self e gamma = typecheckMulExt self e gamma

typecheckBool :: forall repr gamma h.
                 ( Semantics repr
                 , SemanticsMul repr
                 , SemanticsBool repr
                 , Var gamma h)
                => TypeCheck repr gamma h
typecheckBool = typecheckBoolExt typecheckBool

ttExt1, ttExt2 :: Tree
ttExt1 =
    Node "And" [ Node "Bool" [Leaf "t"]
               , Node "Bool" [Leaf "f"]
               ]

ttExt2 =
    Node "Mul" [ Node "Int" [Leaf "2"]
               , Node "If" [ Node "Bool" [Leaf "t"]
                           , Node "Int" [Leaf "10"]
                           , Node "Int" [Leaf "20"]
                           ]]

class SemanticsFix repr where
    -- "sFix" is not limited about creating recursive functions,
    -- let's if this can be handled "more properly"
    -- NOTE: we could be having some wrong type on some "h" of the following:
    -- not going to solve this though.
    sFix :: (repr h a -> repr h a) -> repr h a

instance SemanticsFix R where
    sFix f = R $ fx (unR . f . R)
      where
        fx f' = let x = f' x in x

{-

given an non-negative number "x", this returns 10 times of "x"

the equivalent Haskell function is:

mul10' :: OpenRecursive (Int -> Int)
mul10' v1 v0 = if v0 <= 0 then 0 else 10 + v1 (v0 + (-1))

-}
testMul10 :: forall repr h.
             ( Semantics repr
             , SemanticsFix repr
             , SemanticsBool repr)
             => repr h (Int -> Int)
testMul10 =
    sFix $
      \self ->
      lam $
        if_ (z `leq` int 0)
          (int 0)
          (int 10 `add` (s self `app` (z `add` int (-1))))


instance SemanticsFix S where
    sFix e = S $ \h ->
        let self = "self" ++ show h
        in "(fix \\" ++ self ++ " -> " ++ unS (e (S $ const self)) (succ h) ++ ")"


typecheckFixExt :: forall repr.
                    ( Semantics repr
                    , SemanticsMul repr
                    , SemanticsBool repr
                    , SemanticsFix repr ) =>
                OpenRecursive (forall gamma h. Var gamma h => TypeCheck repr gamma h)
typecheckFixExt self (Node "Fix" [Leaf name, etyp, ebody]) gamma = do
    Typ ta <- readT etyp
    -- parse and typecheck body of the function using extended "gamma"
    -- the problem with this is that the body might not be type-checked alone,
    -- taking a look at "testMul10", and we will notice that unlike other examples,
    -- the "self" variable is introduced by the outer function.
    -- there might be a solution to this, but it doesn't seem I can come up with one
    -- so let's just skip this problem for now.
    DynTerm tbody body <- self ebody (VarDesc ta name, gamma)
    let resultTy = ta `tarr` ta
    case safeGCast tbody body resultTy of
        Just body' ->
            -- body' :: repr (t,h) (t->t)
            -- selfM :: repr h t
            -- as we have multiple things called "self" at the same time
            pure (DynTerm ta (sFix $ \selfM -> undefined))
        Nothing -> Left "type mismatch"
    -- pure (DynTerm ta (sFix _))
typecheckFixExt self e gamma = typecheckBoolExt self e gamma

