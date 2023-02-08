{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Ast.hs
-}

module Ast (
  Ast (Define, Value, Function, Call, Operator),
  cptToAst,
  Operator (Plus, Minus, Times, Div),
) where

import Cpt (Cpt (Literal, Symbol, List), getSymbol)
import Literal (Literal)

type Name = String

type Params = [String]

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value Literal           -- Value, either Bool, Int, Fraction or Float
  | Function Params Ast         -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  | Operator Operator [Ast]     -- Basic operators with its parameters
  deriving (Show)

instance Eq Ast where
  (==) (Value x) (Value y) = x == y
  (==) (Function pas a) (Function pbs b) = pas == pbs && a == b
  (==) (Call a pas) (Call b pbs) = pas == pbs && a == b
  (==) (Operator opa pas) (Operator opb pbs) = pas == pbs && opa == opb
  (==) (Define x a) (Define y b) = x == y && a == b
  (==) _ _ = False


listToParams :: [Cpt] -> Maybe Params
listToParams = mapM getSymbol

listToArgs :: [Cpt] -> Maybe [Ast]
listToArgs = mapM cptToAst

symbolToOperator :: String -> Maybe Operator
symbolToOperator "+" = Just Plus
symbolToOperator "-" = Just Minus
symbolToOperator "*" = Just Times
symbolToOperator "/" = Just Div
symbolToOperator _ = Nothing

symbolToAst :: String -> Maybe Ast
symbolToAst s = Just (Call s [])

defineToAst :: [Cpt] -> Maybe Ast
defineToAst [Symbol a, b] = cptToAst b >>= (Just . Define a)
defineToAst [List (Symbol n:ps), b] = listToParams ps >>=
    (\params -> cptToAst b >>= (Just . Function params)) >>= (Just . Define n)
defineToAst _ = Nothing

listToAst :: [Cpt] -> Maybe Ast
listToAst (Symbol "define":ps) = defineToAst ps
listToAst [Symbol "lambda", List ps, b] = listToParams ps >>= (\params ->
  cptToAst b >>= (Just . Function params))
listToAst (Symbol s:ps) = case symbolToOperator s of
  Just op -> listToArgs ps >>= (Just . Operator op)
  _ -> listToArgs ps >>= (Just . Call s)
listToAst _ = Nothing

cptToAst :: Cpt -> Maybe Ast
cptToAst (Literal i) = Just (Value i)
cptToAst (Symbol s) = symbolToAst s
cptToAst (List l) = listToAst l
