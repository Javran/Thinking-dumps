module Day1Medium

-- spotted this when looking at talk slides
data Parity : Nat -> Type where
  Even : (n : Nat) -> Parity (n+n)
  Odd  : (n : Nat) -> Parity (S (n+n))

parity : (n : Nat) -> Parity n
parity Z = Even Z -- 0 is an even number
parity (S k) with (parity k)
  parity (S (plus n n)) | (Even n) = Odd n -- looks misleading
  parity (S (S (plus n n))) | (Odd n) = Even (S n)

-- parameterized data type representing a binary tree
data BinTree a = Leaf a | Branch (BinTree a) a (BinTree a)

bt1 : BinTree Integer
bt1 = Branch (Branch (Leaf 1) 2 (Leaf 3))
             4
             (Branch (Leaf 5) 6 (Leaf 7))

bt2 : BinTree Char
bt2 = Branch (Branch (Branch (Leaf 'a') 'b' (Leaf 'c'))
                     'd'
                     (Leaf 'e'))
             'f'
             (Leaf 'g')

binTreeToList : BinTree a -> List a
binTreeToList (Leaf v) = [v]
binTreeToList (Branch l x r) = binTreeToList l ++ [x] ++ binTreeToList r
