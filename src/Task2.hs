{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Task2 where

import Task1 (Parse, Parse(..))
import Data.Char (isDigit)

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving Show

-- * Parsing

instance Parse Integer where
  parse s
    | all isDigit s = Just (read s)
    | otherwise     = Nothing

instance Parse IntOp where
  parse "+" = Just Add
  parse "*" = Just Mul
  parse "-" = Just Sub
  parse _   = Nothing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
--
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse s = case parseTokens (words s) [] of
              Just [res] -> Just res
              _          -> Nothing
    where
      parseTokens :: [String] -> [Expr a op] -> Maybe [Expr a op]
      parseTokens [] stack = Just stack
      parseTokens (tok:toks) stack
        | Just x <- parse tok = parseTokens toks (Lit x : stack)
        | Just op <- parse tok = case stack of
                                    (x:y:xs) -> parseTokens toks (BinOp op y x : xs)
                                    _        -> Nothing
        | otherwise = parseTokens toks (Var tok : stack)
      

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

instance Eval Integer IntOp where
  evalBinOp Add = (+)
  evalBinOp Mul = (*)
  evalBinOp Sub = (-)

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
--
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _ (Lit x)           = Just x
evalExpr vars (Var name)     = lookup name vars
evalExpr vars (BinOp op x y) = case (evalExpr vars x, evalExpr vars y) of
                                  (Just nx, Just ny) -> Just (evalBinOp op nx ny)
                                  _                  -> Nothing


-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
--
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate @_ @IntOp

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'forall a op.' part is required to define generic type
-- of intermediate 'Expr' expression that uses scoped type variables 'a' and 'op'.
--
evaluate :: forall a op. (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
evaluate m s = case parse s of
  Just e -> evalExpr m (e :: Expr a op)
  Nothing -> Nothing

