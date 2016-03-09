module Day1Medium

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
