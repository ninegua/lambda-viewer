-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 2.

-- Datatype definition for lambda expression.
data Term = Var Var | Lam Var Term | App Term Term 
data Var  = V String


