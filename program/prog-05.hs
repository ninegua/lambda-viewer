-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 5.

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

-- Parser for variables that start with an alphabet, followed by a number.
variable = mapP f (alpha &&& digits)
  where f (c, d) = V (c : d)
        alpha = char isAlpha
        digits = many1 (char isDigit)

-- Parser for single character variables. 
variable1 = mapP (\c -> V [c]) (char isAlpha)

-- Basic parser combinators.
nil :: ReadS [a]
nil s = [([], s)]

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

mapP :: (a -> b) -> ReadS a -> ReadS b
mapP f g = map (\ (c, s) -> (f c, s)) . g

(&&&) :: ReadS a -> ReadS b -> ReadS (a, b)
f &&& g = \s -> [ ((x, y), s2) 
                | (x, s1) <- f s, 
                  (y, s2) <- g s1 ]

-- Left biased choice (which is actually not appropriate)
(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
f ||| g = \s -> case f s of
                  [] -> map right (g s)
                  xs -> map left xs
  where left  (x, s) = (Left  x, s)
        right (y, s) = (Right y, s)

(<|>) :: ReadS a -> ReadS a -> ReadS a
f <|> g = mapP select (f ||| g)
  where select (Left  x) = x
        select (Right y) = y

many, many1 :: ReadS a -> ReadS [a]
many r  = many1 r <|> nil
many1 r = mapP cons (r &&& many r)
  where cons (x, xs) = x : xs


