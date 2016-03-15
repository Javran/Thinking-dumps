module Day3Medium

data Vector : (n : Nat) -> Type -> Type where
  Nil : Vector Z a
  Cons : a -> Vector l a -> Vector (S l) a

-- (5) add "total" just to make sure it's total
total
vectorAdd : Vector n a -> Vector m a -> Vector (n+m) a -- (1) C-c C-s
-- (2) C-c C-c on "x"
vectorAdd [] y = y -- (3) C-c C-a
vectorAdd (Cons x z) y = Cons x (vectorAdd z y) -- (4) C-c C-a

-- see Day3Easy for the second exercise
