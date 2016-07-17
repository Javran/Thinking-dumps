{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies #-}
module Problem85
  ( Graph
  , mkGraph
  , findIsoMaps
  , iso
  , bigIso
  , degreeTable
  ) where

{-
  NOTE:
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
import Control.Monad.State
import qualified DisjointSet as DS
import DisjointSetState

import Graph
import Utils
import Problem80

-- for this problem we keep not just the graph itself but the list of all edges of it.
-- we could have inferred it from the graph itself but having the redundant info around
-- will make things easier.
type Graph a = (GraphPart a, [Edge a])
type GraphPart a = AdjForm a (Edge a)

mkGraph :: Ord a => [a] -> [(a,a)] -> Graph a
mkGraph vs es = (graphFormToAdjForm (GraphForm vSet eSet), es')
  where
    convert = map (uncurry Edge)
    es' = convert es
    vSet = S.fromList vs
    eSet = S.fromList es'

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
            test (Edge l1 l2) = M.member l1 newVsMap && M.member l2 newVsMap
    -- now we need to check consistencies for all edges in es1L
    es2R <- fix (\ loop curEs1L curEs2 ->
      case curEs1L of
          [] -> pure curEs2
          (Edge l1 l2:es1L') -> do
              let (Just r1) = M.lookup l1 newVsMap
                  (Just r2) = M.lookup l2 newVsMap
                  -- notice that by our impl, "Edge a b == Edge b a" is True
                  e' = Edge r1 r2
              guard $ e' `elem` curEs2
              let newEs2 = delete e' curEs2
              loop es1L' newEs2
          ) es1L es2
    search (es1R,es2R) grps (v1s,v2s) newVsMap

-- | find mappings between two graphs that can prove they are isomorphic
findIsoMaps :: (Ord a, Ord b) => Graph a -> Graph b -> [M.Map a b]
findIsoMaps (ga,eas) (gb,ebs) = do
    let dta = degreeTable ga
        dtb = degreeTable gb
        dtPair = checkDegreeTables dta dtb
    guard $ isJust dtPair
    let dtp = fromJust dtPair
    search (eas,ebs) dtp ([],[]) M.empty

-- | test whether two graphs are isomorphic
iso :: (Ord a, Ord b) => Graph a -> Graph b -> Bool
iso ga gb = not . null $ findIsoMaps ga gb


-- | divide an undirected graph into its connected components.
findConnectedComponents ::
       forall a graphs. (graphs ~ M.Map (S.Set a) (Graph a),  Ord a)
    => Graph a -> [Graph a]
findConnectedComponents (AdjForm g,es) = M.elems subgraphs2
  where
    vs = M.keys g
    subgraphVS :: [S.Set a]
    subgraphVS =
          map S.fromList
        . DS.toGroups
        $ execState findComponents M.empty
      where
        findComponents = initM vs >> mapM_ (withEdge unionM) es
    vToSet :: M.Map a (S.Set a)
    vToSet = M.fromList (concatMap f subgraphVS)
      where
        f :: S.Set a -> [(a,S.Set a)]
        f s = map (,s) (S.toList s)
    subgraphs0 :: graphs
    subgraphs0 = M.fromList (map (,emptyG) subgraphVS)
      where
        emptyG = (AdjForm M.empty, [])
    -- insert mappings into subgraphs
    subgraphs1 :: graphs
    subgraphs1 = foldr update subgraphs0 (M.toList g)
      where
        update (v,eSet) = M.adjust f (fromJust (M.lookup v vToSet))
          where
            f (AdjForm sg, subes) = (AdjForm (M.insert v eSet sg), subes)
    -- insert edges into subgraphs
    subgraphs2 :: graphs
    subgraphs2 = foldr update subgraphs1 es
      where
        update e@(Edge a _) = M.adjust f (fromJust (M.lookup a vToSet))
          where
            f (g', subes) = (g', e:subes)

digestConnectedComponents :: [Graph a] -> M.Map (Int,Int) [Graph a]
digestConnectedComponents = M.fromListWith (++) . map digest
  where
    digest m@(AdjForm g, es) = ((M.size g, length es), [m])

{-
  "bigIso" does the same thing as "iso", but
  would have better performance on graphs that consist of
  more than one connected component and many vertices.

  what it does is:
  - first divide graph into connected components, the idea is that each
    connected component can be checked independently.
  - group connected components by number of edges an vertices,
    so that we don't waste time checking two connected component that
    doesn't have matching number of vertices and edges.
  - for testing isomorphism on each pair of connected components,
    "iso" is used.
-}
bigIso :: forall a b. (Ord a, Ord b) => Graph a -> Graph b -> Bool
bigIso ga gb = isJust $ do
    guard (M.keysSet ga' == M.keysSet gb')
    let pairedGroups = zip (M.elems ga') (M.elems gb')
    mapM (uncurry isoWithinGroup) pairedGroups
  where
    ga' = digestConnectedComponents . findConnectedComponents $ ga
    gb' = digestConnectedComponents . findConnectedComponents $ gb
    isoWithinGroup :: [Graph a] -> [Graph b] -> Maybe ()
    isoWithinGroup [] [] = Just ()
    isoWithinGroup (gl1:gls) gr@(_:_) = listToMaybe $ do
        (gr1,grs) <- pick gr
        guard $ iso gl1 gr1
        maybeToList $ isoWithinGroup gls grs
    isoWithinGroup _ _ = Nothing
