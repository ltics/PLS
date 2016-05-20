module LYAA where

-- inductive definition of natural numbers
data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + m    = m
(suc n) + m = suc (n + m)

-- C-c C-n (suc (suc (suc zero))) + (suc (suc zero)) → (suc (suc (suc (suc (suc zero)))))
-- C-c C-d (suc (suc (suc zero))) + (suc (suc zero)) : ℕ

data IsEven : ℕ → Set where
  evenZ     : IsEven zero
  evenSS    : (n : ℕ) → IsEven n → IsEven (suc (suc n))
  -- evenSS'   : ∀ n     → IsEven n → IsEven (suc (suc n))
  -- evenSS''  : {n : ℕ} → IsEven n → IsEven (suc (suc n))
  -- evenSS''' : ∀ {n}   → IsEven n → IsEven (suc (suc n))


-- C-c C-d evenZ : IsEven zero
-- C-c C-d evenSS zero evenZ : IsEven (suc (suc zero))
-- C-c C-d evenSS' zero evenZ : IsEven (suc (suc zero))
-- C-c C-d evenSS'' evenZ : IsEven (suc (suc zero))
-- C-c C-d evenSS''' evenZ : IsEven (suc (suc zero))
-- and evenSS (suc zero) evenZ is not allowed, zero != (suc zero) of type ℕ, type dependent on value

-- define function use pattern match
even+ : {n m : ℕ} → IsEven n → IsEven m → IsEven (n + m)
even+ evenZ         em = em
even+ (evenSS n en) em = evenSS _ (even+ en em)

-- C-c C-d even+ evenZ evenZ : IsEven zero
-- C-c C-d even+ (evenSS zero evenZ) evenZ : IsEven (suc (suc zero))

one : ℕ
one = suc zero

two : ℕ
two = suc (suc zero)

three : ℕ
three = suc (suc (suc zero))

twoIsEven : IsEven two
twoIsEven = evenSS zero evenZ

-- threeIsEven : IsEven three
-- we can not define the threeIsEven theorem cause there is no value of (IsEven one).

-- C-c C-d
-- even+ twoIsEven (evenSS zero evenZ) : IsEven (suc (suc (suc (suc zero))))
-- even+ (evenSS zero evenZ) twoIsEven : IsEven (suc (suc (suc (suc zero))))

-- use mixfix to define even mixfix 都需要是小写 不然在parse的时候会出现歧义

data _even : ℕ → Set where
  ZERO : zero even
  -- STEP : ∀ x → x even → suc (suc x) even
  STEP : ∀ {x} → x even → suc (suc x) even

-- prove Four is an even number

proof₁ : suc (suc (suc (suc zero))) even
-- proof₁ = STEP _ (STEP _ ZERO)
proof₁ = STEP (STEP ZERO)

-- a function is an implication which the type of its arguments 

{-
A logic consists of a set of axioms and a set of rules.
Axioms are the foundation of the logic: they’re the basic, simple statements that are assumed to be true.
Rules describe how to produce new theorems from existing ones. A theorem is a proposition that has been proved.
Rules are also called Inference Rules which have premises and conclusion.
for a specific Rule if we can prove all the premises, than we can prove the Conclusion.
and a Axiom is just a Rule without premises, the conclusion is true without any proof.

all these logic concept relate to agda

As types are judgments, and values are theorems, data constructors for a type correspond to inference rules for the corresponding proposition.
an implication proposition corresponds to a function type.

-}
