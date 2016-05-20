module Sets.Indexed where

open import Data.Empty using (⊥)
open import Data.Unit  using (⊤; tt)
open import Data.Sum   using (_⊎_; inj₁; inj₂)
open import Data.Bool  using (Bool; true; false)
open import Data.Nat   using (ℕ; zero; suc)

-- Fin family of finite sets

data Fin : ℕ → Set where
  fzero : (n : ℕ) → Fin (suc n)
  fsuc  : (n : ℕ)  → Fin n → Fin (suc n)

{-
zero 0 : Fin 1

zero 1 : Fin 2
suc 1 (zero 0) : Fin 2

zero 2 : Fin 3
suc 2 (zero 1) : Fin 3
suc 2 (suc 1 (zero 0)) : Fin 3

zero 3 : Fin 4
suc 3 (zero 2) : Fin 4
suc 3 (suc 2 (zero 1)) : Fin 4
suc 3 (suc 2 (suc 1 (zero 0))) : Fin 4
-}

-- So we can conclude that Fin n has n distinct elements.

-- Vec A n is an n-tuple of elements of A

data Vec (A : Set) : ℕ → Set where
  [] : Vec A zero
  cons : (n : ℕ) → A → Vec A n → Vec A (suc n)

-- C-c C-d cons 0 true [] → Vec Bool (suc 0)

-- 其实上面这两个已经是dependent type的应用了
