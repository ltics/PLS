module Sets.Propositions where

open import Data.Nat using (ℕ; zero; suc)

data ⊤ : Set where
  tt : ⊤

data ⊥ : Set where

-- Proofs as data

-- conjunction 合取

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 4 _,_
infixr 2 _×_

-- disjunction it only requires one proof in order to hold

data _⊎_ (A B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

infixr 1 _⊎_

⊤×⊤ : ⊤ × ⊤
⊤×⊤ = tt , tt

-- We wish to represent proofs of propositions n ≤ m (n, m = 0, 1, ...).
-- For this we define a set indexed with two natural numbers:

data _≤_ : ℕ → ℕ → Set where
  z≤n : {n : ℕ} → zero ≤ n
  s≤s : {m : ℕ} → {n : ℕ} → m ≤ n → suc m ≤ suc n

infix 4 _≤_

{-
This yields the statements

z≤n {0} : 0 ≤ 0
z≤n {1} : 0 ≤ 1
z≤n {2} : 0 ≤ 2
...
s≤s (z≤n {0}) : 1 ≤ 1
s≤s (z≤n {1}) : 1 ≤ 2
s≤s (z≤n {2}) : 1 ≤ 3
...
s≤s (s≤s (z≤n {0})) : 2 ≤ 2
s≤s (s≤s (z≤n {1})) : 2 ≤ 3
s≤s (s≤s (z≤n {2})) : 2 ≤ 4
...
...
which means that the following propositions have proofs:

0 ≤ 0
0 ≤ 1,  1 ≤ 1
0 ≤ 2,  1 ≤ 2,  2 ≤ 2
0 ≤ 3,  1 ≤ 3,  2 ≤ 3,  3 ≤ 3
...                             ...

Notes

The z≤n constructor yields the first column of statements.
The s≤s constructor yields the successive columns of statements.
1 ≤ 0 is also a valid expression which denotes an empty set.
-}

-- 证明的话就从两个条件z≤n s≤s分别去证明

-- Prove that 3 ≤ 7!

{-
if 3 ≤ 7 would be non-empty, all its elements would look like
1. s≤s x where x : 2 ≤ 6
2. z≤n yields an element in 0 ≤ n and 0 ≠ 3
if 2 ≤ 6 would be non-empty, all its elements would look like
1. s≤s x where x : 1 ≤ 5
2. z≤n yields an element in 0 ≤ n and 0 ≠ 2
...
if 0 ≤ 4 would be non-empty, all its elements would look like
1. s≤s yields an element in suc m ≤ suc n and suc m ≠ 0 某一个自然是的后继必然不能是0
2. z≤n yields an element in 0 ≤ n and 0 = 0
-}

-- prove that a set like 7 ≤ 3 is empty

{-
If 7 ≤ 3 would be non-empty, all its elements would look like
1. s≤s x where x : 6 ≤ 2.
2. z≤n yields an element in 0 ≤ n and 0 ≠ 7.
If 6 ≤ 2 would be non-empty, all its elements would look like
1. s≤s x where x : 5 ≤ 1.
2. z≤n yields an element in 0 ≤ n and 0 ≠ 6.
If 5 ≤ 1 would be non-empty, all its elements would look like
1. s≤s x where x : 4 ≤ 0.
2. z≤n yields an element in 0 ≤ n and 0 ≠ 5.
4 ≤ 0 is empty.
z≤n yields an element in 0 ≤ n and 0 ≠ 4.
s≤s yields an element in suc m ≤ suc n and suc n ≠ 0.
-}

0≤1 : 1 ≤ 10
0≤1 = s≤s z≤n

7≰3 : 7 ≤ 3 → ⊥
7≰3 (s≤s (s≤s (s≤s ())))

8≰4 : 8 ≤ 4 → ⊥
8≰4 (s≤s x) = 7≰3 x -- x是 7 ≤ 3 类型的值 所以传给8≰4需要套上一层s≤s 这样就变成 8 ≤ 4 类型的了

-- Alternative representation

data _≤'_ : ℕ → ℕ → Set where
  ≤'-refl : {m : ℕ} → m ≤' m
  ≤'-step : {m : ℕ} → {n : ℕ} → m ≤' n → m ≤' suc n

infix 4 _≤'_

-- Syntactic abbreviations

{-
Original definition:

data  _≤_ : ℕ → ℕ → Set where
z≤n : {n : ℕ} →                       zero  ≤ n
s≤s : {m : ℕ} → {n : ℕ} →   m ≤ n  →  suc m ≤ suc n
The arrows between typed variables are not needed (also in case of round parenthesis):

data  _≤_ : ℕ → ℕ → Set where
z≤n : {n : ℕ} →                     zero  ≤ n
s≤s : {m : ℕ} {n : ℕ} →   m ≤ n  →  suc m ≤ suc n
Typed variables with the same type can be contracted (also in case of round parenthesis):

data  _≤_ : ℕ → ℕ → Set where
z≤n : {n : ℕ} →               zero  ≤ n
s≤s : {m n : ℕ} →   m ≤ n  →  suc m ≤ suc n
Inferable expressions can be replaced by an underscore:

data  _≤_ : ℕ → ℕ → Set where
z≤n : {n : _} →               zero  ≤ n
s≤s : {m n : _} →   m ≤ n  →  suc m ≤ suc n
Variables with inferred types can be introduced by ∀:

data  _≤_ : ℕ → ℕ → Set where
z≤n : ∀ {n} →               zero  ≤ n
s≤s : ∀ {m n} →   m ≤ n  →  suc m ≤ suc n
-}

-- Addition predicate

data _+_≡_ : ℕ → ℕ → ℕ → Set where
  znn : ∀ {n}     → zero + n ≡ n
  sns : ∀ {m n k} → m + n ≡ k → suc m + n ≡ suc k

-- C-c C-d znn → zero + _n_64 ≡ _n_64
-- C-c C-d sns znn → suc zero + _n_68 ≡ suc _n_68

-- so the Underscores in _+_≡_ denote the space for the operands (mixfix notation).

{-
znn : 0 + 0 ≡ 0
znn : 0 + 1 ≡ 1
znn : 0 + 2 ≡ 2
...
sns znn : 1 + 0 ≡ 1
sns znn : 1 + 1 ≡ 2
sns znn : 1 + 2 ≡ 3
...
sns (sns znn) : 2 + 0 ≡ 2
sns (sns znn) : 2 + 1 ≡ 3
sns (sns znn) : 2 + 2 ≡ 4
...
...
-}

-- Another definition of _≤_

data _≤''_ : ℕ → ℕ → Set where
  ≤+ : ∀ {m n k} → m + n ≡ k → m ≤'' k

-- C-c C-d ≤+ znn → zero ≤'' _n_60

{-
≤+ znn : 0 ≤″ 0
≤+ znn : 0 ≤″ 1
≤+ znn : 0 ≤″ 2
...
≤+ (sns znn) : 1 ≤″ 1
≤+ (sns znn) : 1 ≤″ 2
≤+ (sns znn) : 1 ≤″ 3
...
≤+ (sns (sns znn)) : 2 ≤″ 2
≤+ (sns (sns znn)) : 2 ≤″ 3
≤+ (sns (sns znn)) : 2 ≤″ 4
...
...
-}
