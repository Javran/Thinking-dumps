{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
module Final where

import Data.Function

{-# ANN module "HLint: ignore Eta reduce" #-}

class ExpSYM repr where
    lit :: Int -> repr
    neg :: repr -> repr
    add :: repr -> repr -> repr

instance ExpSYM Int where
    lit = id
    neg n = - n
    add e1 e2 = e1 + e2

instance ExpSYM String where
    lit n = show n
    neg e = "(-" ++ e ++ ")"
    add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"

-- same thing, but in so called "final embedding"
tf1 :: ExpSYM r => r
tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))

eval :: Int -> Int
eval = id

view :: String -> String
view = id

-- note that the type is paramaterized by "r" with a typeclass constraint
tfl1 :: ExpSYM r => [r]
tfl1 = [lit 1, add (lit 1) (lit 2)]

-- to extend the existing expression form, all we have to do
-- is to just define a new typeclass with new operations
class MulSYM repr where
    mul :: repr -> repr -> repr

-- note that clearly if we need the extended form to be evaluated or pretty-printed,
-- we still need to get the corresponding implementation done.
-- but implementing these additional things are not required at all.
instance MulSYM Int where
    mul = (*)

instance MulSYM String where
    mul e1 e2 = "(" ++ e1 ++ " * " ++ e2 ++ ")"

-- the type signature can be inferred easily,
-- here we just make it explicit and not let the compiler complaint about it.
tfm1, tfm2 :: (MulSYM repr, ExpSYM repr) => repr

tfm1 = add (lit 7) (neg (mul (lit 1) (lit 2)))
tfm2 = mul (lit 7) tf1

-- a tree structure that we can serialize expressions from and into.
data Tree
  = Leaf String
  | Node String [Tree]
    deriving (Eq, Read, Show)

instance ExpSYM Tree where
    lit n = Node "Lit" [Leaf $ show n]
    neg e = Node "Neg" [e]
    add e1 e2 = Node "Add" [e1,e2]

toTree :: Tree -> Tree
toTree = id

type ErrMsg = String

safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
    [(x,"")] -> Right x
    _ -> Left $ "Read error: " ++ s

fromTree :: ExpSYM repr => Tree -> Either ErrMsg repr
fromTree t = case t of
    (Node "Lit" [Leaf n]) -> lit <$> safeRead n
    (Node "Neg" [e]) -> neg <$> fromTree e
    (Node "Add" [e1,e2]) -> add <$> fromTree e1 <*> fromTree e2
    _ -> Left $ "Invalid tree: " ++ show t

-- to me it's like actually just making duplications
-- besides there's a type-level information passing going on.
-- the types are pretty much inferred by ghc so there's almost nothing
-- we need to make explicit at type level
instance (ExpSYM repr, ExpSYM repr') => ExpSYM (repr,repr') where
    lit x = (lit x, lit x)
    neg (e1,e2) = (neg e1, neg e2)
    add (e11,e12) (e21,e22) = (add e11 e21, add e12 e22)

-- with (repr, repr') being an instance of ExpSYM (with appropriate)
duplicate :: (ExpSYM repr, ExpSYM repr')
          => (repr, repr') -> (repr, repr')
duplicate = id

checkConsume :: (t -> IO ()) -> Either ErrMsg t -> IO ()
checkConsume _ (Left e) = putStrLn $ "Error: " ++ e
checkConsume f (Right x) = f x

-- duplicate the original piece of data, apply function to it to get a result
-- for printing but also keep a copy of the data
dupConsume :: (Show a, ExpSYM repr, ExpSYM repr')
           => (repr -> a) -> (repr, repr') -> IO repr'
dupConsume ev x = print (ev x1) >> return x2
  where
    (x1,x2) = duplicate x

-- with "dupConsume" we are able to do multiple things at a time
thrice :: (Int, (String, Tree)) -> IO ()
thrice x = dupConsume eval x >>= dupConsume view >>= print . toTree

fromTreeExt :: (ExpSYM repr, func ~ (Tree -> Either ErrMsg repr))
            => func -> func
fromTreeExt self e = case e of
    Node "Lit" [Leaf n] -> lit <$> safeRead n
    Node "Neg" [e'] -> neg <$> self e'
    Node "Add" [e1,e2] -> add <$> self e1 <*> self e2
    _ -> Left $ "Invalid tree: " ++ show e

fromTree' :: ExpSYM repr => Tree -> Either ErrMsg repr
fromTree' = fix fromTreeExt

instance MulSYM Tree where
    mul e1 e2 = Node "Mul" [e1,e2]

instance (MulSYM r1, MulSYM r2) => MulSYM (r1,r2) where
    mul (e11,e12) (e21,e22) = (mul e11 e21, mul e12 e22)

fromTreeExt2 :: (ExpSYM repr, MulSYM repr, func ~ (Tree -> Either ErrMsg repr))
            => func -> func
fromTreeExt2 self (Node "Mul" [e1,e2]) = mul <$> self e1 <*> self e2
fromTreeExt2 self e = fromTreeExt self e -- passing unhandled cases to "fromTreeExt"

fromTree2 :: (ExpSYM repr, MulSYM repr) => Tree -> Either ErrMsg repr
fromTree2 = fix fromTreeExt2

-- make context explicit
-- adding the postfix "PN" to mean "PushNeg" context
data CtxPN = Pos | Neg

instance ExpSYM repr => ExpSYM (CtxPN -> repr) where
    lit n Pos = lit n
    lit n Neg = neg (lit n)
    neg e Pos = e Neg
    neg e Neg = e Pos
    add e1 e2 ctx = add (e1 ctx) (e2 ctx)

instance MulSYM repr => MulSYM (CtxPN -> repr) where
    mul e1 e2 Pos = mul (e1 Pos) (e2 Pos)
    mul e1 e2 Neg = mul (e1 Pos) (e2 Neg) -- push negation only to the second expr

-- interestingly, by type inference this function don't need to have
-- any typeclass constraints
-- however, as soon as any building block of the language is used (lit / neg / add)
-- we are sure to have the corresponding typeclass constraints around.
pushNeg :: (CtxPN -> repr) -> repr
pushNeg e = e Pos

-- LCA: left immediate child of an addition
-- "LCA e" means "Add <hole> e" where "<hole>" is the expression we
-- are dealing with
-- (now I feel the hardest part is to figure out what's the implicit
-- context in initial view that we have to make explicit)
data CtxFlat e = LCA e | NonLCA

-- if any variable in instance declaration below has a postfix "F",
-- that just means that variable should be bound to a function
-- I feel having this postfix will make the code a bit more clear
instance ExpSYM repr => ExpSYM (CtxFlat repr -> repr) where
    lit n NonLCA = lit n
    lit n (LCA e) = add (lit n) e
    neg eF NonLCA = neg (eF NonLCA)
    neg eF (LCA e3) = add (neg (eF NonLCA)) e3
    add e1F e2F ctx = e1F (LCA (e2F ctx))

flatten :: (CtxFlat repr -> repr) -> repr
flatten e = e NonLCA

tf3 :: ExpSYM repr => repr
tf3 = add tf1 (neg (neg tf1))

{- try:

> tf3 :: String
"((8 + (-(1 + 2))) + (-(-(8 + (-(1 + 2))))))"
> (flatten . pushNeg) tf3 :: String
"(8 + ((-1) + ((-2) + (8 + ((-1) + (-2))))))"

-}
