-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 6.
-- 
-- We use the QuickCheck library to help check program correctness.
-- E.g. "quickCheck propTerm" will show that all tests pass for the
-- lambda parser given at the end of this file.
--
-- You can use "quickCheck propSpaced" to test solutions to problem 1,
-- or "quickCheck propPretty" to test solutions to problem 2.

import Data.Char
import Test.QuickCheck
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

-- Randomly generate Var and Term for QuickCheck.
instance Arbitrary Var where
  arbitrary = do
    f <- choose ('a', 'z')
    c <- arbitrary
    d <- choose (0::Int, 100)
    return $ V $ if c then [f] else f : show d

instance Arbitrary Term where
  arbitrary = sized term
    where
      term 0 = Var <$> arbitrary
      term n = oneof [ Var <$> arbitrary,
                       Lam <$> arbitrary <*> term (n-1),
                       App <$> term m <*> term m ]
        where m = n `div` 2


-- Show, Read, Arbitrary instances for Pretty 
data Pretty = Pretty Term deriving Eq

instance Show Pretty where
  show (Pretty x) = pretty x

instance Read Pretty where
  readsPrec = mapP Pretty . readsPrec

instance Arbitrary Pretty where
  arbitrary = fmap Pretty arbitrary

-- Randomly insert spaces into a string.
data Spaced a = Spaced a [Bool] 

instance Eq a => Eq (Spaced a) where
  Spaced x _ == Spaced y _ = x == y

instance Show a => Show (Spaced a) where
  show (Spaced x l) = spaced (show x) l
    where spaced (x:xs) (b:bs) | b && notVar x xs = x : spaced (' ' : xs) bs
                               | otherwise = x : spaced xs bs
          spaced xs [] = xs
          spaced [] _  = []
          notVar x []    = True
          notVar x (y:_) = not (isAlphaNum x && isDigit y)

instance Read a => Read (Spaced a) where
  readsPrec = mapP f . readsPrec 
    where f x = Spaced x []

instance Arbitrary a => Arbitrary (Spaced a) where
  arbitrary = Spaced <$> arbitrary <*> arbitrary

-- To check if the parser can parse all non-ambiguously printed terms.
propTerm :: Term -> Bool
propTerm t = read (show t) == t

-- To check if the parser can parse all pretty printed terms.
propPretty :: Pretty -> Bool
propPretty t = read (show t) == t

-- To check if the parser can handle arbitrarily spaced terms.
propSpaced :: Spaced Term -> Bool
propSpaced t = read (show t) == t

-- To check if the parser can handle arbitrarily spaced pretty terms.
propSpacedPretty :: Spaced Pretty -> Bool
propSpacedPretty t = read (show t) == t

-- ===========================================================
-- You may modify the parser below to complete the assignment.
-- ===========================================================

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

