module Term_Inference where

open import Data.Empty using (⊥)
open import Data.Unit  using (⊤; tt)
open import Data.Sum   using (_⊎_; inj₁; inj₂)
open import Data.Nat   using (ℕ; zero; suc)

-- The Agda compiler tries to infer terms marked with underscore.
-- If the choice of term is ambiguous, term inference fails.
-- 这一系列的pl都拥有inference的能力

data Fin' : ℕ → Set where
  zero : (n : _) → Fin' (suc n) -- ℕ is inferred
  suc  : (n : _)  → Fin' n → Fin' (suc n) -- ℕ is inferred

-- dependent type here!

x : Fin' 3
x = suc _ (zero _) -- 2 and 1 are inferred

-- C-c C-d x → Fin' 3

-- Implicit arguments
-- Underscores can be hidden:
-- Make arguments of constructors implicit with curly brackets.

data Fin : ℕ → Set where
  zero : {n : _} → Fin (suc n)
  suc  : {n : _}  → Fin n → Fin (suc n)

x' : Fin 3
x' = suc {_} (zero {_})

-- C-c C-d x' → Fin 3

-- {_} can be deleted

-- 类型依赖数值
x'' : Fin 3
x'' = suc zero

-- Syntactic abbreviations

{-
data Fin′ : ℕ → Set where
zero : (n : _) → Fin′ (suc n)
suc  : (n : _) → Fin′ n → Fin′ (suc n)

data Fin : ℕ → Set where
zero : {n : _} → Fin (suc n)
suc  : {n : _} → Fin n → Fin (suc n)
Variables with inferred types can be introduced by ∀:

data Fin′ : ℕ → Set where
zero : ∀ n → Fin′ (suc n)
suc  : ∀ n → Fin′ n → Fin′ (suc n)

data Fin : ℕ → Set where
zero : ∀ {n} → Fin (suc n)
suc  : ∀ {n} → Fin n → Fin (suc n)
-}
