module ProofElab

import Language.Reflection.Elab

data Natural = Zero | Suc Natural

plus : Natural -> Natural -> Natural
plus Zero y = y
plus (Suc x) y = Suc (plus x y)

plusCommutes : (x : Natural) -> (y : Natural) -> plus x y = plus y x
plusCommutes Zero y = ?plusCommutes_0_y
plusCommutes (Suc x) y = let hypothesis = plusCommutes x y in
  ?plusCommutes_Sx_y

plusZero : (x : Natural) -> plus x Zero = x
plusZero Zero = ?plusZero_0
plusZero (Suc x) = let hypothesis = plusZero x in
  ?plusZero_Sx

plusSuc : (x : Natural) -> (y : Natural) -> Suc (plus x y) = plus x (Suc y)
plusSuc Zero y = ?plusSuc_0_y
plusSuc (Suc x) y = let hypothesis = plusSuc x y in
  ?plusSuc_Sx_y

plusZero_0 = %runElab (do search)

plusZero_Sx = %runElab (do
  intro'
  intro'
  rewriteWith (Var "hypothesis")
  search)
