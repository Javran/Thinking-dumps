{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies #-}
module Problem93 where

import Control.Arrow
import Data.Maybe
import Data.Ratio
import Data.Function
import Control.Monad
import qualified Data.Set as S

type R = Ratio Int
data OpTyp = Add | Sub | Mul | Div
  deriving (Enum, Show)

data Exp
  = N Int -- a number
  | Op OpTyp Exp Exp -- an operation

instance Show Exp where
    show = pprExp 0

-- | all possible ways of splitting a list into 2 parts
splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits as@(x:xs) = ([],as) : map (first (x:)) (splits xs)

-- | take 2 consecutive elements from a list, with context (left part + right part)
take2s :: forall a. [a] -> [((a,a),([a],[a]))]
take2s xs = mapMaybe convert $ splits xs
  where
    convert :: ([a],[a]) -> Maybe ((a,a),([a],[a]))
    convert (ls,rs) = case rs of
        (r1:r2:rs') -> Just ((r1,r2),(ls,rs'))
        _ -> Nothing

opTypToFunc :: OpTyp
            -> ((r ~ Ratio a, Integral a) => r -> r -> r)
opTypToFunc op = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
      Div -> (/)

opTypPriority :: OpTyp -> Int
opTypPriority op = case op of
    Add -> 1
    Sub -> 1
    Mul -> 2
    Div -> 2

opTypToStr :: OpTyp -> String
opTypToStr op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

eval :: Exp -> R
eval (N i) = fromIntegral i
eval (Op ot l r) = (opTypToFunc ot `on` eval) l r

pprExp :: Int -> Exp -> String
pprExp _ (N n) = show n
pprExp outerPrio (Op ot l r) =
    if outerPrio >= p
       then '(' : content ++ ")"
       else content
  where
    p = opTypPriority ot
    content = pprExp p l ++ opTypToStr ot ++ pprExp p r

-- to solve this problem, we try every possible ways of combining two consecutive
-- expressions and inserting result expression in-place.
-- when applying this process repeatly, we will eventually get exactly 2 expressions.
-- we then evaluate and compare the result of these expressions to determine if the
-- requirement is satisfied.
solve :: [Exp] -> [ (Exp,Exp) ]
solve [l,r] = do
    -- exactly 2 expressions, just need to compare the results of evaluating them
    guard $ eval l == eval r
    pure (l,r)
solve xs@(_:_:_) = do
    -- try all possible ways of combining 2 consecutive expressions into one.
    ((l,r),(as,bs)) <- take2s xs
    op <- [Add .. Div]
    solve (as ++ Op op l r : bs)
solve _ = error "solve: list should contain at least 2 elements"

puzzle :: [Int] -> [String]
puzzle xs =
    removeDuplicates
  . map (\(x,y) -> show x ++ " = " ++ show y)
  $ solve (map N xs)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = S.toList . S.fromList
