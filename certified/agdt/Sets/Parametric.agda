module Sets.Parametric where
open import Data.Nat

-- Set表示类型的类型 所以类型都是Set的自类型
-- 定义类型的时候可以有其他的类型参数

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

-- List A ∈ Set, where A ∈ Set. We call A a parameter of List and we can refer to A in the definition of the set elements

infixr 5 _::_

-- C-c C-d 1 :: [] → List ℕ

-- Cartesian Product 笛卡尔积

-- (A B : Set) is the way of specifying a set that is parameterised by two sets.
data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 4 _,_
infixr 2 _×_

-- C-c C-d 1 , 1 → ℕ × ℕ

data _⊎_ (A B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

infixr 1 _⊎_

-- mutually recursive sets

data List₁ (A B : Set) : Set
data List₂ (A B : Set) : Set

data List₁ (A B : Set) where
  []   : List₁ A B
  _::_ : A → List₂ A B → List₁ A B

data List₂ (A B : Set) where
  _::_ : B → List₁ A B → List₂ A B

-- non regular recursive set

data AlterList (A B : Set) : Set where
  []     : AlterList A B
  _::_ : A → AlterList B A → AlterList A B

-- nested sets are special non regular sets and can be translated to mutually recursive regular sets

data T4 (A : Set) : Set where
  quad : A → A → A → A → T4 A

-- C-c C-d quad 1 2 3 4 → T4 ℕ

data Square (A : Set) : Set where
  zero : A             → Square A -- 2^0 rows
  suc  : Square (T4 A) → Square A -- 2^(n+1) rows

-- 不用管语义 只要类型正确即可

x : Square ℕ
x = suc (suc (zero (quad (quad 1 2 3 4) (quad 4 5 6 7) (quad 9 10 11 12) (quad 13 14 15 16))))
