-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 4.

import Data.Char

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

-- Read instance for Var.
instance Read Var where
  readsPrec _ = variable   

-- Parser for single character variable.
variable s = map f (char isAlpha s)
  where f (c, s1) = (V [c], s1)

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

variable2 s = [ (V [c1,c2], s2) 
              | (c1, s1) <- char isAlpha s 
              , (c2, s2) <- char isAlpha s1 ]
