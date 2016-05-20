module Constants where

open import Sets.Enumerated using (Bool; true; false)
open import Sets.Recursive using (N; zero; suc)

-- constants definitions

nine : N
nine = suc (suc (suc (suc (suc (suc (suc (suc (suc zero))))))))

-- type signature is optional
ten : N
ten = suc nine
