{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Alphametics
  ( solve
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.State.Strict
import Data.Char
import Data.Coerce
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map.Merge.Strict as MMerge
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

data Op = Add | Mul | Expon deriving (Show)

data Expr a
  = Num a
  | Bin Op (Expr a) (Expr a)
  deriving (Functor, Show)

-- Substitutes Expr bottom-up.
substExpr
  :: (opTmp -> r -> r -> r)
  -> (Op -> opTmp)
  -> (n -> r)
  -> Expr n
  -> r
substExpr onBin onOp onNum = fix $ \f -> \case
  Num v -> onNum v
  Bin op l r -> onBin (onOp op) (f l) (f r)

data PuzzleNum = PuzzleNum
  { pLen :: Int
  , -- | most significant digit (which needs to be non-zero).
    pMsd :: Maybe Char
  , -- | digits starting from least significant digit,
    --   which can already be a digit or require lookup.
    pDigits :: [Either Char Int]
  }
  deriving (Show)

mkPuzzleNum :: String -> PuzzleNum
mkPuzzleNum [] = error "input must be non-empty"
mkPuzzleNum raw@(pMsd' : _) = PuzzleNum {pLen, pMsd, pDigits}
  where
    pMsd = pMsd' <$ guard (isAsciiUpper pMsd')
    pLen = length raw
    pDigits = reverse $ fmap tr raw
      where
        tr ch =
          if isDigit ch
            then Right $ ord ch - ord '0'
            else Left ch

type CharAssign = M.Map Char Int

{-
  get Int from a PuzzleNum:
  - Just i: up to (inclusive) column i
  - Nothing: get the whole number
 -}
pzToInt :: Maybe Int -> CharAssign -> PuzzleNum -> Maybe Int
pzToInt mayUpToCol m PuzzleNum {pDigits} =
  foldM
    (\acc i ->
       (acc * 10 +) <$> case i of
         Left ch -> m M.!? ch
         Right d -> pure d)
    0
    (reverse digits)
  where
    digits =
      (case mayUpToCol of
         Nothing -> id
         Just i -> take (i + 1))
        pDigits

evalExpr :: Maybe Int -> CharAssign -> Expr PuzzleNum -> Maybe Int
evalExpr mayUpToCol m = substExpr liftA2 opToFn (pzToInt mayUpToCol m)
  where
    opToFn = \case
      Add -> (+)
      Mul -> (*)
      Expon -> (^)

parse :: String -> Maybe (Expr PuzzleNum, PuzzleNum)
parse raw = case readP_to_S (puzzleP <* eof) raw of
  [(v, "")] -> pure v
  _ -> Nothing
  where
    lm = (<* skipSpaces)
    puzzleP = (,) <$> exprP <*> (lm (string "==") *> numP)
    numP = lm $ mkPuzzleNum <$> munch1 (\ch -> isAsciiUpper ch || isDigit ch)
    exprP =
      makeExprParser
        (Num <$> lm numP)
        [ [simpleBin "^" Expon]
        , [simpleBin "*" Mul]
        , [simpleBin "+" Add]
        ]
      where
        simpleBin sym op = InfixR (Bin op <$ lm (string sym))

-- `[] a` but with element-wise zip and keeping the longest one.
newtype Elemwise a = Elemwise [a] deriving (Show)

instance Semigroup a => Semigroup (Elemwise a) where
  Elemwise (x : xs) <> Elemwise (y : ys) =
    Elemwise $ x <> y : coerce (Elemwise xs <> Elemwise ys)
  Elemwise xs <> Elemwise [] = Elemwise xs
  Elemwise [] <> Elemwise ys = Elemwise ys

{-
  The idea is to cut down search space sooner rather than later.
  Let's focus on least significant digit (LSD) of the result and work
  towards the most significant one (since we don't have carrying
  to worry about starting from LSD).

  Let's define a "column" to be a 0-based index counting from LSDs,
  for each step (i) of the search, starting from i = 0

  - we fill in all digits needed by LHS of column i

  - compute LHS as if those numbers consists of only (i+1) columns

  - now assumption here is that column [0..i] of LHS result must match that part of RHS,
    we check for that and:

    + kill this branch if RHS cannot match.
    + if RHS does match, we can continue. In addition, we might establish some new bindings
      from match result, that we can add to the current set of bindings before continuing.

  - we'll do a final check after all chars needed by LHS are bound,
    and then we can establish remaining bindings by matching against RHS.
 -}
solve :: String -> Maybe [(Char, Int)]
solve raw = do
  (pzLhs, pzRhs) <- parse raw
  let getMsds n =
        maybe S.empty S.singleton (pMsd n)
      {-
        lhsNeedsPre is a list of sets representing Chars needed
        for evaluating that column (least significant digit is the head).

        For example, if we have:

        >     HE
        > + SEES
        > +  THE
        >   ----
        >  LIGHT

        lhsNonZeros will be: {H,S,T}
        lhsNeedsPre will be: [{E,S}, {H,E}, {E,T}, {S}]

        and we do another round of processing on lhsNeedsPre,
        so that already requied numbers from previous columns
        are no longer checked:

        initLhsNeeds will be: [{E,S}, {H}, {T}, {}]
       -}
      _lhsResults :: (S.Set Char, Elemwise (S.Set Char))
      _lhsResults@(lhsNonZeros, Elemwise lhsNeedsPre) =
        -- `instance Semigroup (_,_)` to save us from traversing the tree multiple times.
        substExpr (const (<>)) const num pzLhs
        where
          {-
            collects two things from LHS:
            - most significant digit, which needs to not be zero
            - set of Chars required to complete that "column",
              where a column starts at 0 counting from least significant digit.
           -}
          num :: PuzzleNum -> (S.Set Char, Elemwise (S.Set Char))
          num = (,) <$> getMsds <*> getElemwise
          getElemwise n =
            Elemwise $
              fmap
                (either S.singleton (const S.empty))
                (pDigits n)
      nonZeros = S.union lhsNonZeros (getMsds pzRhs)
      initLhsNeeds = unfoldr go (lhsNeedsPre, S.empty)
        where
          go ([], _) = Nothing
          go (x : xs, alreadyNeeded) =
            Just
              ( S.difference x alreadyNeeded
              , (xs, S.union alreadyNeeded x)
              )
  let {-
        Evaluates LHS (with an optiional column limit)
        and matches it against corresponding parts of the RHS.

        Updates the assignments from a successful match result.
       -}
      verify :: CharAssign -> IS.IntSet -> Maybe Int -> [] (CharAssign, IS.IntSet)
      verify assignments unusedDigits mayUpToCol = do
        lhsVal <- maybeToList $ evalExpr mayUpToCol assignments pzLhs
        let truncatedLhsVal =
              (case mayUpToCol of
                 Just curCol ->
                   take (curCol + 1)
                 Nothing -> id)
                $ revDecimal lhsVal <> repeat 0
            expectedAssigns = M.fromList $ do
              (eDigit, val) <- zip (pDigits pzRhs) truncatedLhsVal
              case eDigit of
                Left ch -> pure (ch, val)
                Right _ -> []
        let performMerge =
              MMerge.mergeA
                -- preserve existing elememts
                MMerge.preserveMissing
                -- remove new element from unusedDigits, and preserve it
                (MMerge.traverseMissing $ \k x -> do
                   guard $ S.notMember k nonZeros || x /= 0
                   True <- gets (IS.member x)
                   modify (IS.delete x)
                   pure x)
                -- if present on both sides, it has to be consistent.
                (MMerge.zipWithMaybeAMatched (\_k l r -> Just l <$ guard (l == r)))
                assignments
                expectedAssigns
        runStateT performMerge unusedDigits

      {-
        Note: `assignments` is authoritative on what digits are unused,
        and `unusedDigits` should be kept in sync with it.
       -}
      dfs :: CharAssign -> [S.Set Char] -> IS.IntSet -> Int -> [] CharAssign
      dfs assignments lhsNeeds unusedDigits curCol =
        case lhsNeeds of
          [] -> do
            (assignments', _unusedDigits) <- verify assignments unusedDigits Nothing
            pure assignments'
          needs : lhsNeeds' ->
            {-
              extracting a random value, being minimum is not required,
              just using this nice destructing interface.
             -}
            case S.minView needs of
              Nothing -> do
                -- all needed digits are set for curCol
                (assignments', unusedDigits') <- verify assignments unusedDigits (Just curCol)
                dfs assignments' lhsNeeds' unusedDigits' (curCol + 1)
              Just (curChar :: Char, needs') ->
                case assignments M.!? curChar of
                  Nothing -> do
                    let nonZero = S.member curChar nonZeros
                    digit <- do
                      d <- IS.toList unusedDigits
                      guard $ not nonZero || d /= 0
                      pure d
                    let assignments' = M.insert curChar digit assignments
                    dfs assignments' (needs' : lhsNeeds') (IS.delete digit unusedDigits) curCol
                  Just _ ->
                    -- this would happen if verification has filled this Char.
                    dfs assignments (needs' : lhsNeeds') unusedDigits curCol
  fmap M.toList
    <$> listToMaybe
    $ dfs M.empty initLhsNeeds (IS.fromDistinctAscList [0 .. 9]) 0

revDecimal :: Int -> [Int]
revDecimal 0 = [0]
revDecimal x = unfoldr f x
  where
    f 0 = Nothing
    f n = let (q, r) = n `quotRem` 10 in Just (r, q)
