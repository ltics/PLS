module Syntax.Decimal_Naturals where

-- use Peano representation of natural number is hard to read if greater than three
-- Agda keep the Peano representation while giving a decimal notation for natural numbers
-- 3 will be a shorthand for suc (suc (suc zero))

open import Data.Nat public using (ℕ; zero; suc)

-- import了标准库里的自然数解决方案就可以用C-c C-d来得到3的类型是ℕ
