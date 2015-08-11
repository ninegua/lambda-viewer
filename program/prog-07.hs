-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 7.
--
-- You can base your solution either on this file, or on your previous solution 
-- for Chapter 6, assuming you have an improved parser for lambda terms.

import Data.Char
import Control.Applicative hiding (many, (<|>))

-- Datatype definition for lambda expression.
data Term = Var Var | Lam Var Term | App Term Term deriving Eq
data Var  = V String deriving Eq

-- Show instances for Var and Term.
instance Show Var where
  show (V s) = s

instance Show Term where
  show (Var v) = show v
  show (Lam x e) = "(λ" ++ show x ++ "." ++ show e ++ ")"
  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"

-- Pretty-print that minimizes the number of parentheses.
-- (solution to problem 2 in tutorial 3)
pretty = snd . fold i g h
  where 
    i (V v)       = (either id id, v)
    g (V v) (_,e) = (either pr pr, "λ" ++ v ++ "." ++ e)
    h (b,f) (d,e) = (either id pr, b (Left f) ++ " " ++ d (Right e))
    pr s = "(" ++ s ++ ")" 

-- Generic fold on Term, used by pretty.
fold :: (Var -> a) -> (Var -> a -> a) -> (a -> a -> a) -> Term -> a
fold i g h = fold'
  where
    fold' (Var v)   = i v
    fold' (Lam v e) = g v (fold' e)
    fold' (App f e) = h (fold' f) (fold' e)

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

(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
f ||| g = \s -> map left (f s) ++ map right (g s)
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

paren p = mapP f (sym '(' &&& p &&& sym ')')
  where f ((_, x), _) = x

-- Read instance for Var and Term.
instance Read Var where
  readsPrec _ = variable 

instance Read Term where
  readsPrec _ = term 

-- Parser for variables that start with lowercase letter,
-- and optionally followed by a number.
variable = mapP f (alpha &&& (digits <|> nil))
  where f (c, d) = V (c : d)
        alpha = char (\c -> c >= 'a' && c <= 'z')
        digits = many1 (char isDigit)

-- Parser for lambda terms that is not ambiguous, and not left-recursive.
term, term', atom :: ReadS Term
term  = mapP (foldl1 App) (many1 term')
term' = mapP fst (atom &&& (space ||| nil))
atom  = lam <|> var <|> paren term

var = mapP Var variable

lam = mapP f (lbd &&& variable &&& sym '.' &&& term)
   where f (((_, v), _), e) = Lam v e

lbd = (sym '\\' <|> sym 'λ')

sym = char . (==)

space = many1 (sym ' ')

-- Free variable set
freeVars :: Term -> [Var]
freeVars = aux []
  where
    aux env (Var v) | elem v env = [] 
                    | otherwise  = [v]
    aux env (Lam v e) = aux (v:env) e
    aux env (App f e) = aux env f ++ aux env e


-- Substitute a free varible by another
subV :: Var -> Var -> Term -> Term
subV u w (Var v)   | v == u    = Var w
                   | otherwise = Var v
subV u w (Lam v e) | v == u    = Lam v e
                   | otherwise = Lam v (subV u w e)
subV u w (App f e) = App (subV u w f) 
                         (subV u w e)
