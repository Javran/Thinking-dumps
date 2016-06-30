{-# LANGUAGE ScopedTypeVariables #-}
module Problem85 where

{-

  plan:
  - search by purposing partial bijections, reject immediately
    if the iso cannot be established in the given way
  - group nodes by their degrees, by which we can reduce search space.

-}


import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Arrow hiding (loop)
import Control.Monad
import Data.List
import Data.Function
import Data.Maybe

import Graph
import Utils
import Problem80

-- for this problem we keep not just the graph itself but the list of all edges of it.
-- we could have inferred it from the graph itself but having the redundant info around
-- will make things easier.
type Graph a = (AdjForm a (Edge a), [Edge a])

mkGraph :: Ord a => [a] -> [(a,a)] -> AdjForm a (Edge a)
mkGraph vs es = graphFormToAdjForm (GraphForm vSet eSet)
  where
    vSet = S.fromList vs
    eSet = S.fromList . map (uncurry Edge) $ es

degreeTable :: forall a b. Ord a => AdjForm a b -> [(Int,[a])]
degreeTable (AdjForm g) =
        M.toAscList
      . M.map S.toList
      . M.fromListWith mappend
      . map (\(v,d) -> (d,S.singleton v))
      $ degrees
  where
    -- the degree of a vertex is the number of edges connected to it.
    -- (undirected)
    degrees :: [(a, Int)]
    degrees = M.toList $ M.map S.size g

-- for 2 graphs to be isomorphic, the degree table must match
-- returns more properly structured data when succeeded: [(Int, ([a],[b]))]
-- in which first Int is still the degree, and ([a],[b]) are 2 lists of the same size.
-- if 2 graphs are indeed isomorphic, then vertices' mapping must be established inside
-- of every group.
-- note that it's not necessary for vertices of 2 graphs to have the same type
-- as long as the mapping can be established (which means at least "Ord" needs to
-- be supported by the corresponding type), everything should work.
checkDegreeTables :: [(Int,[a])] -> [(Int,[b])] -> Maybe [(Int, ([a],[b]))]
checkDegreeTables dt1 dt2
    | convert dt1 == convert dt2 = Just (zip (map fst dt1)
                                             (zip (map snd dt1)
                                                  (map snd dt2)))
    | otherwise = Nothing
  where
    convert = map (second length)

{-
  we need to keep track of many things
  it's not necessary, but let's organize them in pairs:

  * es1 & es2: edges to be verified
  * grps: remaining groups of same degrees
  * curGp1 & curGp2: current group we are working on

-}
search :: forall a b deg. (Ord a, Eq b) =>
          ([Edge a], [Edge b]) ->
          [(deg, ([a],[b]))] ->
          ([a],[b]) ->
          M.Map a b ->
          [M.Map a b]
search (es1,es2) [] ([],[]) vsMap = do
    -- let's put it in invariant that
    -- at the beginning of every call to search
    -- es1 & es2 should already have all verified edges removed
    -- so at this point we can just test emptiness of them
    -- instead of querying vsMap for (unnecessarily) verification
    guard $ null es1 && null es2
    pure vsMap
search ess ((_,(gp1,gp2)):grps') ([],[]) vsMap =
    -- when current group of vertices are done,
    -- we move our focus to the next group
    search ess grps' (gp1,gp2) vsMap
search (es1,es2) grps (curGp1,curGp2) vsMap = do
    (v2,v2s) <- pick curGp2
    let (v1:v1s) = curGp1
        newVsMap = M.insert v1 v2 vsMap :: M.Map a b
        (es1L, es1R) = partition test (es1 :: [Edge a])
          where
            test (Edge l1 l2)
                  -- when both ends can be found in new vsMap
                | Just _ <- M.lookup l1 newVsMap
                , Just _ <- M.lookup l2 newVsMap = True
                | otherwise = False
    -- now we need to check consistencies for all edges in es1L
    es2R <- fix (\ loop curEs1L curEs2 ->
      case curEs1L of
          [] -> pure curEs2
          (Edge l1 l2:es1L') -> do
              let (Just r1) = M.lookup l1 newVsMap
                  (Just r2) = M.lookup l2 newVsMap
              guard $ Edge r1 r2 `elem` curEs2 || Edge r2 r1 `elem` curEs2
              let newEs2 = delete (Edge r1 r2) $ delete (Edge r2 r1) curEs2
              loop es1L' newEs2
          ) es1L es2
    search (es1R,es2R) grps (v1s,v2s) newVsMap

{-
TODO: turn them into tests
seems working, example:

let g1 = mkGraph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
let g2 = mkGraph  [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
let d1 = degreeTable g1
let d2 = degreeTable g2
let (Just groups) = checkDegreeTables d1 d2
let es1 = map (uncurry Edge) [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
let es2 = map (uncurry Edge) [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
search (es1,es2) groups ([],[]) M.empty

example2:

let g1 = mkGraph  [1,2,3,4,5,6,7,8] [(1,2),(1,3),(2,3),(4,3),(5,6),(6,7),(4,5),(5,8),(4,7)]
let g2 = mkGraph  [1,2,3,4,5,6,7,8] [(8,5),(6,4),(4,5),(5,7),(7,6),(4,2),(1,2),(3,2),(1,3)]
let d1 = degreeTable g1
let d2 = degreeTable g2
let (Just groups) = checkDegreeTables d1 d2
let es1 = map (uncurry Edge) [(1,2),(1,3),(2,3),(4,3),(5,6),(6,7),(4,5),(5,8),(4,7)]
let es2 = map (uncurry Edge) [(8,5),(6,4),(4,5),(5,7),(7,6),(4,2),(1,2),(3,2),(1,3)]
search (es1,es2) groups ([],[]) M.empty

-}

-- | find mappings between two graphs that can prove they are isomorphic
findIsoMaps :: (Ord a, Ord b) => Graph a -> Graph b -> [M.Map a b]
findIsoMaps (ga,eas) (gb,ebs) = do
    let dta = degreeTable ga
        dtb = degreeTable gb
        dtPair = checkDegreeTables dta dtb
    guard $ isJust dtPair
    let dtp = fromJust dtPair
    search (eas,ebs) dtp ([],[]) M.empty
