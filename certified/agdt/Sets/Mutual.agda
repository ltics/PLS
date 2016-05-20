module Sets.Mutual where

open import Sets.Enumerated using (Bool; true; false)
open import Syntax.Decimal_Naturals using (ℕ; zero; suc)

-- 相互递归定义 你中有我 我中有你

data L : Set
data M : Set

data L where
  nil  : L
  _::_ : ℕ → M → L

data M where
  _::_ : Bool → L → M
