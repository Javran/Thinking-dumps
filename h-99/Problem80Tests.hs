{-# LANGUAGE FlexibleInstances, TupleSections, TemplateHaskell #-}

import Test.QuickCheck
import Control.Applicative

import Graph
import Problem80 hiding (main)

prop_GraphFormToAdjForm :: GraphForm Char (Edge Char) -> Property
prop_GraphFormToAdjForm = (===) <$> id
                                <*> adjFormToGraphForm
                                  . graphFormToAdjForm


prop_AdjFormToGraphForm :: AdjForm Char (Edge Char) -> Property
prop_AdjFormToGraphForm = (===) <$> id
                                <*> graphFormToAdjForm
                                  . adjFormToGraphForm

prop_GraphFormToFndForm :: GraphForm Char (Edge Char) -> Property
prop_GraphFormToFndForm = (===) <$> id
                                <*> fndFormToGraphForm
                                  . graphFormToFndForm

prop_FndFormToGraphForm :: FndForm Char (Edge Char) -> Property
prop_FndFormToGraphForm = (===) <$> fndFormToGraphForm
                                <*> fndFormToGraphForm
                                  . graphFormToFndForm
                                  . fndFormToGraphForm

return []
main :: IO Bool
main = $quickCheckAll
