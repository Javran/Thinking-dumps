module Problem89 where

{-
  core idea taken from:
  https://en.wikipedia.org/wiki/Bipartite_graph#Testing_bipartiteness

  - perform depth search and color nodes, during which time check for consistency.
  - note that it might be cases where the graph is not a single connected component,
    and in that case we need to check if every connected component.
-}
