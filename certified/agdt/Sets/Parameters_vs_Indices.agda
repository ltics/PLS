module Sets.Parameters_vs_Indices where

open import Data.Nat  using (ℕ; zero; suc; _≤_; z≤n; s≤s)
open import Data.List using (List; []; _∷_) --这个双冒号是unicode 打之前需要先打正斜杠

{-
data _≤′_ : ℕ → ℕ → Set where
  ≤′-refl : {m : ℕ} →                       m ≤′ m
  ≤′-step : {m : ℕ} → {n : ℕ} →  m ≤′ n  →  m ≤′ suc n

is similar to

data _≤′_ (m : ℕ) : ℕ → Set where
  ≤′-refl :                       m ≤′ m
  ≤′-step : {n : ℕ} →  m ≤′ n  →  m ≤′ suc n
-}

{-
data _≤″_ : ℕ → ℕ → Set where
  ≤+ : ∀ {m n k} → m + n ≡ k → m ≤″ k

is similar to

data _≤″_ (m : ℕ) : ℕ → Set where
  ≤+ : ∀ {n k} → m + n ≡ k → m ≤″ k

which is similar to

data _≤″_ (m : ℕ) (k : ℕ) : Set where
  ≤+ : ∀ {n} → m + n ≡ k → m ≤″ k
-}

-- A parameter instead of an index is always a better choice

-- General equality

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

-- C-c C-d refl {ℕ} {0} : 0 ≡ 0 类型就是一个judgement

data _∈_ {A : Set} (x : A) : List A → Set where
  first : ∀ {xs} → x ∈ x ∷ xs
  later : ∀ {y xs} → x ∈ xs → x ∈ y ∷ xs

infix 4 _∈_

-- C-c C-d first {ℕ} {1} : 1 ∈ 1 ∷ _xs_6
-- C-c C-d later {ℕ} {1} : 1 ∈ _xs_10 → 1 ∈ _y_9 ∷ _xs_10
-- C-c C-d later (first {ℕ} {1}) : 1 ∈ _y_23 ∷ 1 ∷ _xs_25
