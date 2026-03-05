{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings
--
{-# OPTIONS_GHC -Wno-orphans #-}

module Task3 where

import Task1 (Parse(..))
import Task2 (Eval(..), Expr, evalExpr)

import Data.List (nub)

instance Parse Bool where
  parse "true"  = Just True
  parse "false" = Just False
  parse _       = Nothing

data BoolOp = And | Or | Xor
  deriving Show

instance Parse BoolOp where
  parse "and" = Just And
  parse "or"  = Just Or
  parse "xor" = Just Xor
  parse _     = Nothing

instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or  = (||)
  evalBinOp Xor = (/=)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT s = case parse s of
  Just (expr :: Expr Bool BoolOp) -> Just $ elem (Just True) $ map (`evalExpr` expr) $ getCombinations $ getVars s
  Nothing     -> Nothing

getVars :: String -> [String]
getVars = nub . filter (`notElem` ["and", "or", "xor"]) . words

getCombinations :: [String] -> [[(String, Bool)]]
getCombinations [] = [[]]
getCombinations (x:xs) = [(x, b) : rest | b <- [True, False], rest <- getCombinations xs]

