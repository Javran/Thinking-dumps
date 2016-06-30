{-# LANGUAGE ScopedTypeVariables #-}
module Problem85Test where

import Test.Hspec
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Function
import Control.Monad
import Data.Monoid

import Problem85
import Graph

{-# ANN module "HLint: ignore Redundant do" #-}

deleteOrFail :: Eq a => a -> [a] -> Maybe [a]
deleteOrFail _ [] = Nothing
deleteOrFail v (x:xs) =
    if v == x
       then Just xs
       else (x:) <$> deleteOrFail v xs

-- verify isomorphic relation by testing whether the proposed mapping
-- can be used to establish mappings of all edges.
verifyIso :: (Ord a, Ord b) => Graph a -> Graph b -> M.Map a b -> Bool
verifyIso (_,es1) (_,es2) m = isJust $
    -- make use of Maybe monad and then convert it to Bool
    fix (\ loop curEs1 curEs2 -> case curEs1 of
             -- no more edges to be verified,
             -- see if there are still edges in the other
             -- (it should not be if this is indeed isomorphic)
             [] -> guard $ null curEs2
             (Edge l1 l2:newCurEs1) -> do
                 -- test edge l1-l2 by:
                 -- (1) try to get corresponding vertices r1 and r2 through m
                 r1 <- M.lookup l1 m
                 r2 <- M.lookup l2 m
                 -- (2) attempt to remove the correponding edge from the other graph
                 let attempt1 = deleteOrFail (Edge r1 r2) curEs2
                     attempt2 = deleteOrFail (Edge r2 r1) curEs2
                 newCurEs2 <- getFirst (First attempt1 <> First attempt2)
                 -- at this point, we can establish the correspondence between
                 -- l1-l2 and r1-r2
                 loop newCurEs1 newCurEs2
         ) es1 es2

main :: IO ()
main = hspec $ do
    describe "findIsoMaps" $ do
        specify "example 1" $ do
            let g1 = mkGraph
                       [1 :: Int,2,3,4,5,6,7,8]
                       [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8)
                       ,(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
                g2 = mkGraph
                       [1 :: Int,2,3,4,5,6,7,8]
                       [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7)
                       ,(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
                result = findIsoMaps g1 g2
            result `shouldSatisfy` all (verifyIso g1 g2)
        specify "example 2" $ do
            let g1 = mkGraph
                       [1 :: Int,2,3,4,5,6,7,8]
                       [(1,2),(1,3),(2,3),(4,3),(5,6),(6,7),(4,5),(5,8),(4,7)]
                g2 = mkGraph
                       [1 :: Int,2,3,4,5,6,7,8]
                       [(8,5),(6,4),(4,5),(5,7),(7,6),(4,2),(1,2),(3,2),(1,3)]
                result = findIsoMaps g1 g2
            result `shouldSatisfy` all (verifyIso g1 g2)
        specify "example 3 (non-iso)" $ do
            let g1 = mkGraph
                       ['A'..'F']
                       [('A','B'),('B','C'),('C','E'),('E','F'),('B','D'),('D','E')]
                g2 = mkGraph
                       [1 :: Int .. 6]
                       [(5,1),(1,6),(2,3),(3,4),(1,3),(4,6)]
                result = findIsoMaps g1 g2
            result `shouldSatisfy` null
