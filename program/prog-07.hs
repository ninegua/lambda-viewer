-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 7.
--
-- You can base your solution either on this file, or on your previous solution 
-- for Chapter 6, assuming you have an improved parser for lambda terms.

import Data.Char
import Control.Applicative hiding (many, (<|>))
import Test.QuickCheck
import Data.List (intersect)

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
 
-- Randomly generate Var and Term for QuickCheck.
instance Arbitrary Var where
  -- use a limited range to increase chances for variable re-use
  arbitrary = fmap (V . (:[])) $ choose ('a', 'z')

instance Arbitrary Term where
  arbitrary = sized term
    where
      term 0 = Var <$> arbitrary
      term n = oneof [ Var <$> arbitrary,
                       Lam <$> arbitrary <*> term (n-1),
                       App <$> term m <*> term m ]
        where m = n `div` 2

-- Testing for alphaEq's behavior
propAlpha :: Int -> Int -> Term -> Bool
propAlpha i j e = nlam == 0 || (testEq e e1 && testEq e1 e) &&
                    (null fvs || testNEq e1 e2 && testNEq e2 e1 &&
                                 testNEq e1 e3 && testNEq e3 e1) &&
                    (null fvs' || testNEq e4 e4' && testNEq e4' e4 &&
                                  testEq e5 e5' && testEq e5' e5)
  where
    testEq e e' = alphaEq e e' || error ("Expect True, but alphaEq (" ++ show e ++ ") (" ++ show e' ++ ") is False")
    testNEq e e' = not (alphaEq e e') || error ("Expect False, but alphaEq (" ++ show e ++ ") (" ++ show e' ++ ") is True")
    -- count the number of lambdas
    nlam = fold (\_ -> 0) (\_ _ -> 1) (+) e 
    n = abs i `mod` nlam
    navigate i f e@(Lam v e') = if i == n then Left (e, f) else navigate (i + 1) (f . Lam v) e'
    navigate i f e@(App e1 e2) = 
      case navigate i (f . flip App e2) e1 of
        Right i' -> navigate i' (f . App e1) e2
        r -> r
    navigate i f _ = Right i
    Left (Lam v e', ctx) = navigate 0 id e
    bvs = boundVars e'
    fvs = filter (/=v) $ freeVars e'
    -- a case that is always valid
    u1 = head [ u | u <- allVars, u `notElem` bvs, u `notElem` fvs ]
    e1 = ctx $ Lam u1 (subV v u1 e')
    -- a case that is always invalid
    u2 = fvs !! (abs j `mod` length fvs)
    e2 = ctx $ Lam u2 (subV v u2 e')
    -- a case that is always invalid
    u3 = fvs !! (abs j `mod` length fvs)
    e3 = ctx $ Lam v (subV u3 v e')
    -- a case that is always invalid
    fvs' = fvs `intersect` freeVars e
    u4 = fvs' !! (abs j `mod` length fvs')
    e4  = Lam u4 e
    e4' = Lam v $ ctx $ Lam v (subV u4 v e')
    -- a case that is always valid
    (u5:u6:_) = [ u | u <- allVars, u `notElem` freeVars e, u `notElem` boundVars e, u `notElem` fvs, v `notElem` fvs ]
    e5  = Lam u4 e
    e5' = Lam u5 $ subV u4 u5 $ ctx $ Lam u6 (subV v u6 e')

-- An infinite supply of variable names
atoz = ['a' .. 'z']
allVars = map V (map (:[]) atoz ++ [ v : show m | v <- atoz, m <- [0..]])

-- Bounded variable set
boundVars :: Term -> [Var]
boundVars = fold (\_ -> []) (:) (++) 

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
 

-- ===========================================================
-- You may modify the function below to complete the assignment.
-- ===========================================================

alphaEq :: Term -> Term -> Bool
alphaEq e e' = True 

-- You may test the correctness of your implementation with the following:
-- main = quickCheck propAlpha
