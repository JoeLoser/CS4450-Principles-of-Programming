-- Joe Loser
-- CS4450 HW4
-- 11/3/14

import PropLogic

import Control.Applicative
import Control.Monad
import Data.List (nub)

-- #1 - returns the set of all free variables in a proposition
freeVars :: String -> Maybe [Var]
freeVars str = case parse(str) of
  Nothing -> Nothing -- returns Nothing on any error
  Just x -> Just $ nub $ freeVars' x
  where 
  freeVars' (Atom v) = [v]
  freeVars' (Not b) = freeVars' b
  freeVars' (Imply a b) = freeVars' a ++ freeVars' b
  freeVars' (Or a b) = freeVars' a ++ freeVars' b
  freeVars' (And a b) = freeVars' a ++ freeVars' b
  freeVars' (Iff a b) = freeVars' a ++ freeVars' b

-- #2 - evaluates a proposition given an assignment of truth-values to free variables
evalProp :: [(Var, Bool)] -> String -> Maybe Bool
evalProp assoc str = do
  parsed <- parse str
  evalProp1 assoc parsed
  where
  evalProp1 assoc (Atom v) = lookup v assoc
  evalProp1 assoc (Not a) = do 
    a <- evalProp1 assoc a 
    return $ not a
  evalProp1 assoc (Imply a b) = do
    a <- evalProp1 assoc a
    b <- evalProp1 assoc b
    return $ a <= b
  evalProp1 assoc (Or a b) = do
    a <- evalProp1 assoc a
    b <- evalProp1 assoc b
    return $ a || b
  evalProp1 assoc (And a b) = do
    a <- evalProp1 assoc a
    b <- evalProp1 assoc b
    return $ a && b
  evalProp1 assoc (Iff a b) = evalProp1 assoc $ And (Imply a b) (Imply b a)

-- #3 - determines whether a proposition is a tautology (true under every possible free 
-- variable truth-assignment)
  -- idea is to zip all the free vars with the truth table
  -- run through evalProp
  -- check to make sure all of them are true--if so, it's a tautology
isTautology :: String -> Maybe Bool
isTautology str = 
  let
    parsed = parse str -- this is a Maybe Prop
    frees = freeVars str
    -- takes in the list of frees, calcs length of frees and then gets all possible
    -- combinations of those free vars.
    -- arg1 = anon function to apply
    -- arg2 = variables I want to apply arg1 to 
    -- returns Monad of that result of arg1 applied to arg2
    table = liftM (\fs -> truthTable $ length fs) frees
    frees' = liftM (\fs -> replicate (2 ^ length fs) fs) frees
    pairs = liftM2 (\fs bs -> zip fs bs) frees' table
     -- applying zip function to all of the pairs to get a list of association lists
     -- doing pattern matching on the tuple (xs, ys) in the inner lambda function
    assocs = liftM (\ps -> map (\(xs, ys) -> zip xs ys) ps) pairs
    -- partially applying map to the associations
    results = liftM (map (flip evalProp str)) assocs
  in all (== Just True) <$> results -- fm

-- Helper function to generate all the possible values for a given number
-- of variables
truthTable :: Int -> [[Bool]]
truthTable 0 = [[]]
truthTable n = map (False:) prev ++ map (True:) prev 
  where prev = truthTable (n-1)
