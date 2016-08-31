{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Final where

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
