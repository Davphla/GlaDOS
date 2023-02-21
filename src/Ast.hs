{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Ast.hs
-}

module Ast (
  Ast (Define, Value, Lambda, Call, Operator),
  cptToAst,
  listToParams,
  listToAst,
  Params,
) where

import Cpt (Cpt (..), Keyword (..), getIdentifier)
import Literal (Literal)
import Operator (Operator (..))

type Name = String

type Params = [String]


data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value Literal               -- Value, either Bool, Int, Fraction or Float
  | Lambda Params Ast           -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  | Operator Operator [Ast]     -- Basic operators with its parameters
  deriving (Eq, Show)


listToParams :: [Cpt] -> Maybe Params
listToParams = mapM getIdentifier

listToArgs :: [Cpt] -> Maybe [Ast]
listToArgs = mapM cptToAst

-- symbolToOperator :: String -> Maybe Operator
-- symbolToOperator "+" = Just (Operator.Operator Plus 1)
-- symbolToOperator "-" = Just (Operator.Operator Minus 1)
-- symbolToOperator "*" = Just (Operator.Operator Times 2)
-- symbolToOperator "/" = Just (Operator.Operator Div 2)
-- symbolToOperator _ = Nothing

symbolToAst :: String -> Maybe Ast
symbolToAst s = Just (Call s [])

keywordToAst :: Keyword -> Maybe Ast
keywordToAst _ = Nothing

-- defineToAst :: [Cpt] -> Maybe Ast
-- defineToAst [Identifier a, b] = cptToAst b >>= (Just . Define a)
-- defineToAst [List (Identifier n:ps), b] = listToParams ps >>=
--     (\params -> cptToAst b >>= (Just . Function params)) >>= (Just . Define n)
-- defineToAst _ = Nothing

listToAst :: [Cpt] -> Maybe Ast
listToAst [Keyword Cpt.Lambda, List ps, b] = listToParams ps >>= (\params ->
  cptToAst b >>= (Just . Ast.Lambda params))
listToAst (Identifier s:ps) = listToArgs ps >>= (Just . Call s)
-- listToAst _ = Nothing

operatorToAst :: Operator -> Maybe Ast
operatorToAst _ = Nothing

cptToAst :: Cpt -> Maybe Ast
cptToAst (Literal i) = Just (Value i)
cptToAst (Identifier s) = symbolToAst s
cptToAst (List l) = listToAst l
cptToAst (Keyword k) = keywordToAst k
cptToAst (Cpt.Operator o) = operatorToAst o