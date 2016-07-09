{-# LANGUAGE ScopedTypeVariables #-}
module Problem89Test where

import Test.Hspec

import Problem89

{-# ANN module "HLint: ignore Redundant do" #-}

type RawGraph a = ([a],[(a,a)])

graphAppend :: RawGraph a -> RawGraph b -> RawGraph (Either a b)
graphAppend (vs1,es1) (vs2,es2) = (map Left vs1 ++ map Right vs2
                                  , map (both Left) es1 ++ map (both Right) es2)
  where
    both f (a,b) = (f a, f b)

main :: IO ()
main = hspec $ do
    describe "bipartite" $ do
        specify "example 1" $
            bipartite ([1 :: Int,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
                `shouldBe` True
        specify "example 2" $
            bipartite ([1 :: Int,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
                `shouldBe` False
        let evenLoop :: RawGraph Int
            evenLoop = ([1..4],[(1,2),(2,3),(3,4),(4,1)])
            oddLoop :: RawGraph Int
            oddLoop = ([1..3],[(1,2),(2,3),(3,1)])
        specify "example 3" $
            bipartite evenLoop `shouldBe` True
        specify "example 4" $
            bipartite oddLoop `shouldBe` False
        specify "example 5" $
            bipartite (evenLoop `graphAppend` evenLoop) `shouldBe` True
        specify "example 6" $
            bipartite (evenLoop `graphAppend` evenLoop `graphAppend` evenLoop) `shouldBe` True
        specify "example 7" $
            bipartite (evenLoop `graphAppend` oddLoop `graphAppend` evenLoop) `shouldBe` False
