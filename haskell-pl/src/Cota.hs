module Cota where

-- pipeline operation inspired from F# world
(|>) :: b -> (b -> c) -> c
(|>) = flip ($)    
