module Sets.Recursive where

-- Sets.Recursive 递归类型

-- Peano representation of natural numbers
-- it's just a unary presentation of natural number
data N : Set where
  zero : N
  suc  : N -> N

-- a binary presentation of natural number
data N+ : Set where
  one      : N+
  double   : N+ -> N+
  double+1 : N+ -> N+

data N2 : Set where
  zero : N2
  id   : N+ -> N2

data BinTree : Set where
  leaf : BinTree
  node : BinTree -> BinTree -> BinTree


