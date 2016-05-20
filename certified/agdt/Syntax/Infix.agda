module Syntax.Infix where

open import Sets.Enumerated using (Bool; true; false)

-- 中缀操作符

data BinTree' : Set where
  x : BinTree'
  _+_ : BinTree' → BinTree' → BinTree'

infixr 3 _+_ -- _+_操作符友结合优先
