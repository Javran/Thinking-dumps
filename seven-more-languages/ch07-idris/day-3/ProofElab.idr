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
  
sucEq : {n, m : Natural} -> (n = m) -> (Suc n = Suc m)
sucEq Refl = Refl

plusZero_0 = %runElab (do search)

plusZero_Sx = %runElab (do
  intro'
  intro'
  rewriteWith (Var "hypothesis")
  search)
  
plusSuc_0_y = %runElab (do
  intro'
  search)

plusSuc_Sx_y = %runElab (do
  intro'
  intro'
  intro'
  rewriteWith (Var "hypothesis")
  search)
  
plusCommutes_0_y = %runElab (do
  intro'
  rewriteWith (RApp (Var "plusZero") (Var "y"))
  search)

{-
Holes: ProofElab.plusCommutes_Sx_y
*ProofElab> :elab plusCommutes_Sx_y 


----------                 Goal:                  ----------
{hole0} : (x : Natural) ->
          (y : Natural) ->
          (plus x y = plus y x) -> Suc (plus x y) = plus y (Suc x)
-ProofElab.plusCommutes_Sx_y> intro "a"

----------              Assumptions:              ----------
 a : Natural
----------                 Goal:                  ----------
{hole0} : (y : Natural) ->
          (plus a y = plus y a) -> Suc (plus a y) = plus y (Suc a)
-ProofElab.plusCommutes_Sx_y> intro "b"

----------              Assumptions:              ----------
 a : Natural
 b : Natural
----------                 Goal:                  ----------
{hole0} : (plus a b = plus b a) ->
          Suc (plus a b) = plus b (Suc a)
-ProofElab.plusCommutes_Sx_y> intro "H0"

----------              Assumptions:              ----------
 a : Natural
 b : Natural
 H0 : plus a b = plus b a
----------                 Goal:                  ----------
{hole0} : Suc (plus a b) = plus b (Suc a)
-ProofElab.plusCommutes_Sx_y> :eval plusSuc b a
plusSuc b a : Suc (plus b a) = plus b (Suc a)
-ProofElab.plusCommutes_Sx_y> rewriteWith (RApp (RApp (Var "plusSuc") (Var "b"))  (Var "a"))

----------              Assumptions:              ----------
 a : Natural
 b : Natural
 H0 : plus a b = plus b a
----------                 Goal:                  ----------
{hole0} : Suc (plus a b) = Suc (plus b a)
-ProofElab.plusCommutes_Sx_y> rewriteWith (Var "H0")
INTERNAL ERROR: Can't rewrite here
This is probably a bug, or a missing error message.
Please consider reporting at https://github.com/idris-lang/Idris-dev/issues
-ProofElab.plusCommutes_Sx_y> 
-}
