module Proof

-- TODO: investigate how to use Elab afterwards
-- import Language.Reflection.Elab

data Natural = Zero | Suc Natural

plus : Natural -> Natural -> Natural
plus Zero y = y
plus (Suc x) y = Suc (plus x y)

plusCommutes : (x : Natural) -> (y : Natural) -> plus x y = plus y x
plusCommutes Zero y = ?plusCommutes_0_y
plusCommutes (Suc x) y = let hypothesis = plusCommutes x y in
  ?plusCommutes_Sx_y

plusZero : (x : Natural) -> plus x Zero = x

plusSuc : (x : Natural) -> (y : Natural) -> Suc (plus x y) = plus x (Suc y)

plusCommutes_0_y = proof
  intros
  rewrite (sym (plusZero y))
  trivial 

plusCommutes_Sx_y = proof
  intros
  rewrite (plusSuc y x)
  rewrite hypothesis 
  trivial 
