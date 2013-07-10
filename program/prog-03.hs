-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 3.

-- Datatype definition for lambda expression.
data Term = Var Var | Lam Var Term | App Term Term 
data Var  = V String

-- Show instances for Var and Term.
instance Show Var where
  show (V s) = s

instance Show Term where
  show (Var v)   = show v
  show (Lam x e) = "(λ" ++ show x ++ "." ++ show e ++ ")"
  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"
